##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé
# Email: johannes.nowe@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/05_create_env_grids_to_predict.R
# Script Description: create the environmental rasters to predict on
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----load-packages------------------------------------------------------------
library(terra)
library(lubridate)
library(stringr)
library(dplyr)
library(sf)

## ----parameters----------------------------------------------------------------
start <- "2021-01-01"
end <- "2022-12-31"
# Create sequence of dates
dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "day")
doy   <- lubridate::yday(dates)

## ----load-input---------------------------------------------------------------
# Load datasets generated in chunk02
elevation <- terra::rast(file.path
                         (processed_dir, "bathy_rast.nc"))
habitats <- terra::rast(file.path(processed_dir, "habitats_rast.tif"))
sst_daily <- terra::rast(file.path(processed_dir, "sst_rast.nc"))
shipwrecks <- sf::st_read(file.path(processed_dir, "shipwrecks.gpkg"))
OWF <- sf::st_read(file.path(processed_dir, "OWF.gpkg"))

#Load the study area
BPNS <- sf::st_read(file.path(raw_dir, "BPNS.gpkg"))

## ----lonlat-rasters-----------------------------------------------------------

## Explanation: we need raster layers in CRS 4326 but with values that represent the lat/lon in metres (for accurate predictions)

# project grid to EPSG:3035
r_3035 <- terra::project(elevation, "EPSG:3035")

# create projected coordinates (meters)
x_m_3035 <- init(r_3035, "x")
y_m_3035 <- init(r_3035, "y")

# test_y <- bathy %>% project("EPSG:3035") %>% init("y") %>% mask(shipwreck_dist)
# test_x <- bathy %>% project("EPSG:3035") %>% init("x") %>% mask(shipwreck_dist)

# project values back to 4326 grid
x_m_4326 <- terra::project(x_m_3035, elevation, method = "near") %>% terra::mask(elevation)
y_m_4326 <- terra::project(y_m_3035, elevation, method = "near") %>% terra::mask(elevation)

rm(y_m_3035, x_m_3035)

## ----distance-rasters---------------------------------------------------------
OWF_dist_rast <- terra::distance(r, OWF)
OWF_dist_rast <- terra::mask(OWF_dist_rast, terra::vect(BPNS)) #only keep values inside BPNS
names(OWF_dist_rast) <- "Distance to OWF"
varnames(OWF_dist_rast) <- "Distance to OWF"

shipwreck_dist_rast <- terra::distance(r, shipwrecks)
shipwreck_dist_rast <- terra::mask(shipwreck_dist_rast, terra::vect(BPNS))
names(shipwreck_dist_rast) <- "Distance to shipwreck"
varnames(shipwreck_dist_rast) <- "Distance to shipwreck"

terra::plot(OWF_dist_rast, main = "Distance to OWF")
terra::plot(shipwreck_dist_rast, main = "Distance to shipwreck")

## ----lod-raster---------------------------------------------------------------
# #The formula for length of day is daylength(lat, day of year)
# lod <- list()
# for(i in 1:365){
#   lod[[i]] <- setValues(rast(r),geosphere::daylength(lat_vals, i))
# }
# lod <- terra::rast(lod)
# lod <- mask(lod, BPNS)
# time(lod) <- seq(ymd(start),ymd(end), by = '1 day')
# names(lod) <- stringr::str_replace(time(lod), "[0-9]+-", "")
# varnames(lod) <- "Length of day"
# terra::plot(lod[[c(1,90,180,270)]])

# new approach that creates one layer per timestep

# build the raster stack
lod <- purrr::map(doy, ~{
  r %>%
    setValues(geosphere::daylength(lat_vals, .x))
}) %>%
  rast() %>%
  mask(r)
# Assign the time
time(lod) <- dates
#names(lod) <- stringr::str_replace(time(lod), "[0-9]+-", "")
varnames(lod) <- "lod"
names(lod) <- as.character(dates)

## ----n_active_tags raster ----------------------------------------------------
# For the offset in the model, a spatially constant raster with the amount of 
# active transmitters in the water per time step

n_active_tags <- #number of active tags per timestep
  readRDS(file.path(processed_dir, 'detections_day.rds')) %>%
    dplyr::select(time, n_active_tags) %>%
    dplyr::distinct() %>%
    dplyr::select(n_active_tags) %>%
    as.list()

# 2. Create an empty raster with the same properties as BPNS
n_active_tags_rast <- rast(r)
# 3. Set number of layers = number of days
nlyr(n_active_tags_rast) <- length(dates)
# 4. Assign layer names as dates
names(n_active_tags_rast) <- as.character(dates)
# 5. Fill each layer with the corresponding n_active_tags value (spatially constant)
for (i in seq_along(dates)) {
  values(n_active_tags_rast[[i]]) <- n_active_tags$n_active_tags[i]
}
# 6. Assign time dimension
time(n_active_tags_rast) <- dates
# 7. mask with BPNS boundaries
n_active_tags_rast <- terra::mask(n_active_tags_rast, r)

## ----write-results------------------------------------------------------------
#lat
terra::writeCDF(y_m_4326, file.path(processed_dir,"y_m_4326.nc"), varname = "y_m", overwrite = TRUE)
# terra::writeCDF(lat_raster, file.path(processed_dir,"lat_rast.nc"), varname = "lat", overwrite = TRUE)
#lon
terra::writeCDF(x_m_4326, file.path(processed_dir,"x_m_4326.nc"), varname = "x_m", overwrite = TRUE)
# terra::writeCDF(lon_raster, file.path(processed_dir,"lon_rast.nc"), varname = "lon", overwrite = TRUE)
#distance to shipwreck
terra::writeCDF(shipwreck_dist_rast, file.path(processed_dir,"shipwreck_dist_rast.nc"), varname = "distance_to_shipwreck", overwrite = TRUE)
#distance to OWF
terra::writeCDF(OWF_dist_rast, file.path(processed_dir,"OWF_dist_rast.nc"), varname = "distance_to_OWF", overwrite = TRUE)
#length of day
terra::writeCDF(lod, file.path(processed_dir,"lod_rast.nc"), varname = "length_of_day", overwrite = TRUE)
#n_active_tags
terra::writeCDF(n_active_tags_rast, file.path(processed_dir,"n_active_tags_rast.nc"), varname = "n_active_tags", overwrite = TRUE)
