##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/01_detection_data_to_hourly_presences.R
# Script Description: process raw acoustic detections and deployments data to a 
#                     monthly aggregation of presences per station
# SETUP ------------------------------------
 cat("\014")                          # Clears the console
 rm(list = ls())                      # Remove all variables of the work space
 source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----load-packages------------------------------------------------------------
library(dplyr)
library(knitr)
library(tidyr)
library(lubridate)
library(sf)

## ----parameters----------------------------------------------------------------
# First, we define our study period, namely our start and end date
start <- "2021-01-01"
end <- "2022-12-31"

## ----read raw data----------------------------------------------------------------

### ----Belgian EEZ--------------------------------------------------
# Since for now our study area is the Belgian Part of the North Sea, it will be the bounding box
BPNS <- 
  sf::st_read(file.path(raw_dir, "BPNS.gpkg")) 

### ----acoustic-detections--------------------------------------------------

# These are the 'raw' acoustic detections, as accessed via the etn R package (https://github.com/inbo/etn)
detections_raw <- base::readRDS("01_data/01_raw_data/detections.rds") 

# Now, we summarise the detection data per day and get the number of unique individuals detected at each acoustic receiver station.
detections_days <- detections_raw |>
  dplyr::select(time = date_time,
                animal_id,
                deploy_latitude,
                deploy_longitude,
                station_name)|>
  dplyr::mutate(time = lubridate::date(time))|>
  group_by(time, station_name)|>
  distinct(animal_id)|>
  summarise(count = n())


# Now, we summarise the detection data per month and get the number of unique individuals detected at each acoustic receiver station.     
detections_month <- 
  detections_raw |>
      dplyr::mutate(
       month = lubridate::month(date_time))  |>
      dplyr::group_by(month, station_name) |>
      dplyr::summarise(n_dets = dplyr::n(), .groups = "drop",
      n_animals = n_distinct(animal_id) |> as.integer(),
      lat = deploy_latitude |> mean(na.rm = T),
      lon = deploy_longitude |> mean(na.rm = T)) |>
  # make into sf object
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  # ensure we have POINT and not MULTIPOINT data
  sf::st_cast("POINT")

# save the monthly detections dataframe
base::saveRDS(detections_month, file.path(processed_dir, "detections_month.rds"))

### ----animals--------------------------------------------------

# These are the animal metadata, as accessed via the etn R package (https://github.com/inbo/etn)
animals <- base::readRDS("01_data/01_raw_data/animals.rds") 

# These are the tag metadata, as accessed via the etn R package (https://github.com/inbo/etn)
tags <- base::readRDS("01_data/01_raw_data/tags.rds")

# Now we make a simple dataframe with start and end date of each tag
tags_start_end <-
  animals |> 
  dplyr::select(tag_serial_number, tag_date) |>
  dplyr::left_join(tags |> 
                     # only keep one row per tag_serial_number
                     dplyr::distinct(tag_serial_number, battery_estimated_end_date) ) |>
  dplyr::mutate(battery_estimated_end_date = as.Date(battery_estimated_end_date)) |>
  dplyr::rename(tag_start = tag_date,
                tag_end = battery_estimated_end_date)

### ----acoustic_deployments-------------------------------------------------
# These are the acoustic deployment metadata, as accessed via the etn R package (https://github.com/inbo/etn)
deployments <-
  base::readRDS(file.path(raw_dir, "deployments.rds"))

## ----create-time-index--------------------------------------------------------
# Create a time index for each day from start to end
time_index <- 
  tidyr::tibble(time = seq(
    from = as.Date(start),
    to = as.Date(end),
    by = "day"))

## ----active-tags-per-time-index--------------------------------------------------------
# Get the number of active tags per timestep (to later use as parameter in the model)
active_tags <- time_index |>
  mutate(
    n_active_tags = rowSums(
      outer(time, tags_start_end$tag_start, `>=`) &
        outer(time, tags_start_end$tag_end, `<=`)
    )
  )

## ----stations-days------------------------------------------------------------
# Now, we make a dataframe summarising each receiver station into one row
stations <- 
  deployments |>
    sf::st_drop_geometry() |>
    dplyr::group_by(station_name) |>
    dplyr::summarise(lat = deploy_latitude |> mean(na.rm = T),
                     lon = deploy_longitude |> mean(na.rm = T)) |>
  # make into sf object
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  # ensure we have POINT and not MULTIPOINT data
  sf::st_cast("POINT")

base::saveRDS(stations, file.path(processed_dir, "stations.rds"))

## ----deployments-days---------------------------------------------------------

# Now, use the time_index df together with the deployments df to create a new df
# with 1 row per timestep and station
deployments_days <-
  time_index |>
    dplyr::left_join(deployments |>
              dplyr::select(station_name, deploy_date_time, recover_date_time) |>
              sf::st_drop_geometry() |>
              dplyr::mutate(value_deploy = 0,
                            deploy_date = as.Date(deploy_date_time),
                            recover_date = as.Date(recover_date_time)),
    by = dplyr::join_by(between(time, deploy_date, recover_date)))|>
    dplyr::select(-c(deploy_date_time, recover_date_time, deploy_date, recover_date))

base::saveRDS(deployments_days, "01_data/02_processed_data/deployments_days.rds")


## ----merge-dataframes---------------------------------------------------------

# We replace the initial 0 with the count of animals for that day at that station

detections_day <- 
  deployments_days |>
  dplyr::left_join(detections_days, by = c("time", "station_name")) |>
  dplyr::mutate(count = tidyr::replace_na(count, 0)) |>
  dplyr::select(time, station_name, count)


saveRDS(detections_day, file = "01_data/02_processed_data/detections_day.rds")

#Join with information on location and active tags
output_chunk01 <- 
  detections_day |>
  dplyr::left_join(stations, by = join_by(station_name)) |>
  dplyr::left_join(active_tags, by = join_by(time))

## ----save-outputs-------------------------------------------------------------
# as RDS
base::saveRDS(output_chunk01, file.path(processed_dir, "output_chunk01.rds"))