##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/03_couple_env_data_to_detections.R
# Script Description: couple environmental data to processed acoustic detections
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----R-packages---------------------------------------------------------------

## other
library(dplyr)
library(knitr)
library(readr)
library(tidyr)
library(lubridate)
library(units)
library(sf)
library(terra)
library(geosphere)



## ----load-detections----------------------------------------------------------
chunk01 <- 
    base::readRDS(file.path(processed_dir, "output_chunk01.rds")) |>
    # make id number for terra extractions
    dplyr::mutate(row_id = row_number())|>
    dplyr::rename(acoustic_detection = count) |>
    sf::st_sf()

## ----load-stations------------------------------------------------------------
stations <-
    base::readRDS(file.path(processed_dir, "stations.rds"))

## ----load-vector-data---------------------------------------------------------
shipwrecks <- sf::st_read(file.path(processed_dir, "shipwrecks.gpkg"))
OWF <- sf::st_read(file.path(processed_dir, "OWF.gpkg")) |> sf::st_make_valid()
# st_is_valid(chunk01_subset) |> unique()
# bathy <- sf::st_read("../../01_data/02_processed_data/bathy.gpkg")
# habitats <- sf::st_read("../../01_data/02_processed_data/habitats.gpkg")


## ----load-raster-data---------------------------------------------------------
sst_rast <- terra::rast(file.path(processed_dir, "sst_rast.nc"))
# plot(sst[[1]])

bathy_rast <- terra::rast(file.path(processed_dir, "bathy_rast.nc"))

habitats_rast <- terra::rast(file.path(processed_dir, "habitats_rast.tif"))


## ----overlaps-OWF-------------------------------------------------------------
# sf::sf_use_s2(FALSE)

# pipes don't work during rendering so we disconnect the two following statements
OWF_flag <- sf::st_intersects(chunk01, OWF) |> base::lengths() > 0

# calc the distance to the nearest OWF polygon for each station
OWF_distance_stations <- 
    stations |>
    mutate(min_dist_owf = st_distance(geometry, OWF) |>
            apply(1, min) |>
            units::set_units("m") |>  # optional: convert units
            units::drop_units()) 


chunk01_OWF <- 
  chunk01 |>
    mutate(OWF = OWF_flag) |>
    dplyr::left_join(OWF_distance_stations |> st_drop_geometry() |> select(min_dist_owf, station_name), by = "station_name") 


## ----overlaps-shipwrecks------------------------------------------------------
distance <- 100 # in meters

shipwrecks_flag <- sf::st_is_within_distance(chunk01, shipwrecks, dist = distance) |> lengths() > 0

shipwrecks_distance_stations <- 
    stations |>
    mutate(min_dist_shipwreck = st_distance(geometry, shipwrecks) |>
            apply(1, min) |>
            units::set_units("m") |>  # optional: convert units
            units::drop_units()) 

chunk01_shipwrecks <- 
  chunk01 |>
    mutate(shipwreck = shipwrecks_flag) |>
    dplyr::left_join(shipwrecks_distance_stations |> st_drop_geometry() |> select(min_dist_shipwreck, station_name), by = "station_name")

## ----sst-overlap--------------------------------------------------------------

# 1. Create lookup between time and layer index
layer_time_index <- tibble(
  time = time(sst_rast),
  layer_index = seq_along(time(sst_rast))
)

# 2. Join exact match on time to get layer index
chunk01_matched <- 
    chunk01 |>
        dplyr::left_join(layer_time_index, by = "time") #|>
        # dplyr::mutate(row_id = row_number())

# 3. Convert to SpatVector once (for terra::extract)
chunk_vect <- terra::vect(chunk01_matched)

# 4. Extract exact layer values per point using bind = TRUE to preserve metadata
sst_vals <- 
    terra::extract(
        sst_rast,
        chunk_vect,
        layer = chunk01_matched$layer_index#,
        # bind = TRUE  # this retains input attrs like row_id
        ) |>
        dplyr::as_tibble() |>
        dplyr::rename(sst = 3)  # column 3 = SST value

# 5. remove duplicate rows and unpack layer index (for join in next step)
sst_vals_corrected <- 
    sst_vals |>
        distinct() |>
        rename(row_id = ID, layer_index = layer) |>
        mutate(layer_index = as.integer(gsub(".*_", "", layer_index))) |>
        group_by(row_id) |>
        summarise(
            sst = mean(sst, na.rm = TRUE),
            layer_index = unique(layer_index),
            .groups = "drop"
        )

# 6. join the chunk01 output with the extracted sst values
chunk01_sst <- 
    chunk01_matched |>
        left_join(sst_vals_corrected |>
                    dplyr::select(sst, layer_index, row_id), by = c("row_id", "layer_index")) 


## ----bathy-overlaps-----------------------------------------------------------

# Extract raster values and keep the ID
bathy_vals <- terra::extract(bathy_rast, terra::vect(chunk01)) |>
  rename(row_id = ID) |>  # terra returns "ID" col
  group_by(row_id) |>
  # there are severl rows per ID, so we need to summarise them: calc mean
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# Join bathy values back in
chunk01_bathy <- 
    chunk01 |>
        dplyr::left_join(bathy_vals, by = "row_id")

rm(bathy_vals)

## ----habitats-overlaps--------------------------------------------------------

# Extract habitat raster values
habitats_vals <- 
    terra::extract(habitats_rast, terra::vect(chunk01)) |>
     rename(row_id = ID)%>%distinct(row_id, .keep_all = TRUE)

# Join bathy values back in
chunk01_habitats <- 
    chunk01 |>
        dplyr::left_join(habitats_vals, by = "row_id")

rm(habitats_vals)


## ----join-all-----------------------------------------------------------------

output_chunk03_01 <-
    chunk01 |>
        dplyr::bind_cols(chunk01_OWF |> st_drop_geometry() |> select(OWF, min_dist_owf))|>
        dplyr::bind_cols(chunk01_shipwrecks |> st_drop_geometry() |> select(shipwreck, min_dist_shipwreck))|>
        dplyr::bind_cols(chunk01_bathy |> st_drop_geometry() |> select(elevation)) |>
        dplyr::bind_cols(chunk01_habitats |> st_drop_geometry() |> select(habitat)) |>
        dplyr::bind_cols(chunk01_sst |> st_drop_geometry() |> select(sst))

## ----length of day ------------------------------------------------------------

#The formula for length of day is daylength(lat, day of year)
output_chunk03_02 <- output_chunk03_01 |>
  dplyr:: mutate(lod = geosphere::daylength(deploy_latitude, lubridate::yday(time)))


## ----scale------------------------------------------------------------

output_chunk03_03 <- output_chunk03_02 |>
  dplyr:: mutate(sst_scaled = scale(sst) |> as.numeric(),
                 min_dist_owf_scaled = scale(min_dist_owf) |> as.numeric(),
                 elevation_scaled = scale(elevation) |> as.numeric(),
                 lod_scaled = scale(lod) |> as.numeric(),
                 min_dist_shipwreck_scaled = scale(min_dist_shipwreck) |> as.numeric())

## ----lat and lon in meter-based CRS------------------------------------------------------------

purrr::map(output_chunk03_03, class)

output_chunk03_04 <- output_chunk03_03 |>
  ## extract first point from multipoint
  # dplyr:: mutate(
  #   geometry = purrr::map(sf::st_geometry(.), ~
  #                    if (inherits(.x, "MULTIPOINT")) {
  #                      st_point(.x[1, ])  # extract first point as sfg POINT
  #                    } else {
  #                      .x                  # leave POINT as-is
  #                    }))  %>%
      # st_sfc(crs = st_crs(chunk03)))
  sf::st_cast("POINT") %>%
  sf::st_transform(3035) %>%                              # project to metres
  dplyr::mutate(
    x_m = sf::st_coordinates(.)[,1],
    y_m = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_transform(st_crs(output_chunk03_03)) 

## --------save output---------------------------------------------------------------------

# as RDS
base::saveRDS(output_chunk03_04, file.path(processed_dir, "output_chunk03.rds"))
