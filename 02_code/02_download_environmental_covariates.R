##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/02_download_environmental_covariates.R
# Script Description: download environmental data
# SETUP ------------------------------------
# cat("\014")                          # Clears the console
# rm(list = ls())                      # Remove all variables of the work space
# source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----load-packages------------------------------------------------------------

library(reticulate)
library(ncdf4)
library(dplyr)
library(knitr)
library(readr)
library(emodnet.wfs)
library(rerddap)
library(tidyr)
library(lubridate)
library(sf)
library(terra)

#setwd("C:/Users/lotte.pohl/OneDrive - VLIZ/Documents/repositories/dto-bioflow_wp4_duc2/02_code/01_boosted_regression_trees") # set working directory to the folder where the script is located -> same as qmd rendering

## ----geospatial-data----------------------------------------------------------
# BPNS
BPNS <- 
  sf::st_read(file.path(raw_dir, "BPNS.gpkg")) 

## ----variables----------------------------------------------------------------
lon_min <- 2.2
lon_max <- 3.4
lat_min <- 51
lat_max <- 51.9

start <- "2021-01-01"
end <- "2022-12-31"

## ----shipwrecks---------------------------------------------------------------

shipwrecks <- 
    readr::read_delim(file.path(raw_dir, "wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
        dplyr::select(Easting, Northing, `Gezonken op`, Code, Bouwjaar) |>
        sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) |>
        sf::st_transform(4326) |>
    dplyr::bind_cols(
        readr::read_delim(file.path(raw_dir, "wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
        dplyr::select(Easting, Northing) |>
        sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) |>
        sf::st_transform(4326) |>
        sf::st_coordinates() |> # Extract coordinates as a matrix |>
        tidyr::as_tibble() |> # Convert coordinates matrix to tibble
        dplyr::rename(lon = X, lat = Y))

## ----wfs-human----------------------------------------------------------------
# initiate WFS client
wfs_human <- emodnet.wfs::emodnet_init_wfs_client(service = "human_activities")

# inspect available layers
# wfs_human |> emodnet.wfs::emodnet_get_wfs_info() |> View()

OWF <- 
    wfs_human |> 
        emodnet.wfs::emodnet_get_layers(layers = "windfarmspoly", crs = 4326) |>
        purrr::pluck("windfarmspoly") 

## ----erddap-url---------------------------------------------------------------
# This is the url where the EMODnet ERDDAP server is located
erddap_url <- "https://erddap.emodnet.eu/erddap/"

## ----habitats-ERDDAP----------------------------------------------------------
habitats_dataset_id <- "biology_8777_429f_a47a_d420"

# get info on dataset
habitats_info <- rerddap::info(datasetid = habitats_dataset_id, url = erddap_url) 

# fetch dataset
habitats_erddap <- 
  rerddap::griddap(datasetx = habitats_info, 
                   longitude = c(lon_min, lon_max), 
                   latitude = c(lat_min, lat_max))

habitats <- 
    # retain the 'data' col
    habitats_erddap$data |>
    # make an sf obj
    mutate(lat = latitude, lon = longitude) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    # retain habitats inside BPNS
    mutate(within_BPNS = sf::st_within(geometry, BPNS) |> lengths() > 0) |>
    filter(within_BPNS) |>
    select(-within_BPNS)

### ----habitats-rasterise-------------------------------------------------------
habitats_vect <- terra::vect(habitats)  # terra's format for vector data

# Define raster extent, resolution and CRS
template_raster <- terra::rast(
  extent = ext(habitats_vect),
  resolution = 0.01,  #0.0001 set resolution based on desired cell size
  crs = crs(habitats_vect)
)

habitats_rast <- 
  terra::rasterize(habitats_vect, 
                   template_raster, 
                   field = "eusm_benthos_eunis2019ID")
# To make it a categorical spatraster
#the categories are the different found classes without NA
unique_habitats <- unique(habitats_vect$eusm_benthos_eunis2019ID)%>%na.omit()
#todo: Change habitat to the correct names when we get this list
levels(habitats_rast) <- data.frame(id = unique_habitats, habitat = unique_habitats)

## ----erddap-bathy-------------------------------------------------------------

bathy_dataset_id <- "dtm_2020_v2_e0bf_e7e4_5b8f"
#get info on dataset
bathy_info <- rerddap::info(datasetid = bathy_dataset_id, url = erddap_url) 

# fetch dataset
bathy_erddap <- rerddap::griddap(datasetx = bathy_info, longitude = c(2.2, 3.4), latitude = c(51, 51.9))

bathy <- 
    # retain the 'data' col
    bathy_erddap$data |>
    # make an sf obj
    mutate(lat = latitude, lon = longitude) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    # retain habitats inside BPNS
    mutate(within_BPNS = sf::st_within(geometry, BPNS) |> lengths() > 0) |>
    filter(within_BPNS) |>
    select(-within_BPNS)

## ----bathy-rasterise----------------------------------------------------------

bathy_vect <- terra::vect(bathy)  # terra's format for vector data

# Define raster extent, resolution and CRS
template_raster <- terra::rast(
  extent = ext(bathy_vect),
  resolution = 0.01,  #0.0001 set resolution based on desired cell size
  crs = crs(bathy_vect)
)

bathy_rast <- terra::rasterize(bathy_vect, template_raster, field = "elevation")

## ----cmems-sst----------------------------------------------------------------
# this is a dataset with daily resolution and the highest spatial res there currently is available

## 0. define parameters
dataset_id <- "IFREMER-ATL-SST-L4-NRT-OBS_FULL_TIME_SERIE"
variable_name <- "analysed_sst"
start_time <- paste0(start, "T00:00:00")
end_time <- paste0(end, "T23:59:00")

output_path <- processed_dir

library(reticulate)
#virtual environment
virtualenv_create(envname = "CopernicusMarine", force = FALSE)

if("copernicusmarine" %in% reticulate::py_list_packages("CopernicusMarine")$package){
  print("copernicusmarine package already installed")
} else {
  virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
}
use_virtualenv("CopernicusMarine", required = TRUE)
cmt <- reticulate::import("copernicusmarine")

# Read .Renviron
readRenviron(getwd())
cmems_username <- Sys.getenv("CMEMS_USER")
cmems_password <- Sys.getenv("CMEMS_PASS")
cmt$login(cmems_username, cmems_password)
# query the data
cmt$subset(
  dataset_id= dataset_id,
  variables=list(variable_name),
  minimum_longitude=lon_min - 1,
  maximum_longitude=lon_max + 0.5,
  minimum_latitude=lat_min - 0.5,
  maximum_latitude=lat_max + 0.5,
  start_datetime= start_time,
  end_datetime= end_time,
  output_directory = output_path
)

## ----sst-time-----------------------------------------------------------------
sst_rast <- terra::rast(file.path(output_path,
                                  "IFREMER-ATL-SST-L4-NRT-OBS_FULL_TIME_SERIE_analysed_sst_1.21E-3.89E_50.51N-52.39N_2021-01-01-2022-12-31.nc"),
                        subds = variable_name)

# convert time: already POSIXCT so no conversion needed
time_sst <- terra::time(sst_rast)
summary(time_sst)

# convert temperature from Kelvin to Celcius
sst_rast <- sst_rast - 273.15

## ----sst-BPNS-crop------------------------------------------------------------
# Crop and mask the raster to the BPNS extent
sst_rast_BPNS <- sst_rast |> 
  crop(BPNS) |>          # Trim to bounding box
  mask(BPNS, touches = TRUE)             # Mask values outside polygon

## ----save-outputs-------------------------------------------------------------

#shipwrecks
sf::st_write(shipwrecks, file.path(processed_dir,"shipwrecks.gpkg"), delete_dsn = TRUE)

#OWF
sf::st_write(OWF, file.path(processed_dir,"OWF.gpkg"), delete_dsn = TRUE)

#seabed habitats
##tif, netcdf does not retain categories information
terra::writeRaster(habitats_rast, file.path(processed_dir,"habitats_rast.tif"), overwrite = TRUE)

# bathymetry
terra::writeCDF(bathy_rast, file.path(processed_dir,"bathy_rast.nc"), varname = "elevation", overwrite = TRUE)

# sst
terra::writeCDF(sst_rast_BPNS, file.path(processed_dir,"sst_rast.nc"), varname = variable_name, overwrite = TRUE)