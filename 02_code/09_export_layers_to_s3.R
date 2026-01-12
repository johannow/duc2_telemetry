##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé & Lotte Pohl
# Email: johannes.nowe@vliz.be & lotte.pohl@vliz.be
# Date: 2026-01-09
# Script Name: ~/duc2_telemetry/02_code/09_export_layers_to_s3.R
# Script Description: Export the generated layers from the modelling workflow as s3 objects
# to be accessible by e.g. RShiny applications.
# Based on this tutorial by willem boone: https://github.com/DTO-BioFlow/EDITO_data_R/blob/main/S3_operations/3_store_S3_nc.R
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----libraries-------------------------------------------------------------------------
library(dplyr)
library(terra)
library(stars)
library(paws)
library(tidyverse)

## ----load-functions----------------------------------------------------------------
list.files(func_dir, pattern = "\\.R$", full.names = TRUE) |>
  purrr::walk(source)

message("⚠️ Attention! Be sure to update your env variables from this website (https://datalab.dive.edito.eu/account/storage) in the .Renviron file, they expire after 24h.")

## ----parameters----------------------------------------------------------------
start <- "2021-01-01"
end <- "2022-12-31"
# Create sequence of dates
dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "day")

## ----1. load (monthly aggregated) raster layers-------------------------------------------------------------------------
### PREDICTORS ###
# time-invariant rasters are stored in 01_data/02_processed_data
constant_predictors_dir <- processed_dir

bathy <- terra::rast(file.path(constant_predictors_dir, "bathy_rast.nc"))
habitats <- terra::rast(file.path(constant_predictors_dir, "habitats_rast.tif"))
owf_dist <- terra::rast(file.path(constant_predictors_dir, "OWF_dist_rast.nc"))
shipwreck_dist <- terra::rast(file.path(constant_predictors_dir, "shipwreck_dist_rast.nc"))
# lat
y_m_4326 <- terra::rast(file.path(processed_dir,"y_m_4326.nc"))
#lon
x_m_4326 <- terra::rast(file.path(processed_dir,"x_m_4326.nc"))

# per-month aggregated layers of time-variant rasters are stored in the aggregations_dir
predictors_monthly_dir <- file.path(aggregations_dir, "predictors_monthly")


lod_median_months <- terra::rast(file.path(predictors_monthly_dir, "lod_median_months.nc")) %>% suppressWarnings()
sst_median_months <- terra::rast(file.path(predictors_monthly_dir, "sst_median_months.nc")) %>% suppressWarnings()
n_active_tags_median_months <- terra::rast(file.path(predictors_monthly_dir, "n_active_tags_median_months.nc")) %>% suppressWarnings()

### ACOUSTIC TELEMETRY DATA ###
data_monthly_dir <- file.path(aggregations_dir, "median_counts_monthly")

counts_median_months <- terra::rast(file.path(data_monthly_dir, "chunk01_counts_median_months.nc")) %>% suppressWarnings()

### PREDICTIONS ###
predictions_monthly_dir <- file.path(aggregations_dir, "predictions_monthly")

predictions_inside_owf_median_months <- terra::rast(file.path(predictions_monthly_dir, "predictions_inside_owf_median_months.nc")) %>% suppressWarnings()
predictions_outside_owf_median_months <- terra::rast(file.path(predictions_monthly_dir, "predictions_outside_owf_median_months.nc")) %>% suppressWarnings()
# diff_owf <- terra::rast(file.path(pred_dir, "diff_owf.nc")) %>% suppressWarnings() #does not work yet
# temporary solution
diff_owf <- suppressWarnings(predictions_inside_owf_median_months - predictions_outside_owf_median_months)
terra::writeCDF(x = diff_owf,
                filename = file.path(pred_dir, "diff_owf.nc"),
                varname = "difference in predicted count",
                overwrite = TRUE) %>% suppressWarnings()


## ----2. export layer as s3 ------------------------------------------------------

### PREDICTORS ####
# old - from before the function was written - keeping just in case, delete later
# s_bathy <- stars::st_as_stars(bathy)
# names(s_bathy) <- "elevation"
# attr(s_bathy, "units") <- "m"
# attr(s_bathy, "description") <- "bathymetry of the Belgian Part of the North Sea"

export_layer_as_s3(layer = bathy,
                   layer_name = "elevation",
                   layer_units = "m",
                   layer_description = "bathymetry of the Belgian Part of the North Sea",
                   dir = s3_dir)

# s_habitats <- stars::st_as_stars(habitats)
# names(s_habitats) <- "habitat category"
# attr(s_habitats, "units") <- ""
# attr(s_habitats, "description") <- "seabed habitat categories of the Belgian Part of the North Sea"

export_layer_as_s3(layer = habitats,
                   layer_name = "habitat category",
                   layer_units = "",
                   layer_description = "seabed habitat categories of the Belgian Part of the North Sea",
                   dir = s3_dir)

# s_owf_dist <- stars::st_as_stars(owf_dist)
# names(s_owf_dist) <- "distance to closest offshore wind farm"
# attr(s_owf_dist, "units") <- "m"
# attr(s_owf_dist, "description") <- "distance to closest owf in the Belgian Part of the North Sea"

export_layer_as_s3(layer = owf_dist,
                   layer_name = "distance to closest offshore wind farm",
                   layer_units = "m",
                   layer_description = "distance to closest owf in the Belgian Part of the North Sea",
                   dir = s3_dir)

# s_shipwreck_dist <- stars::st_as_stars(shipwreck_dist)
# names(s_shipwreck_dist) <- "distance to closest shipwreck"
# attr(s_shipwreck_dist, "units") <- "m"
# attr(s_shipwreck_dist, "description") <- "distance to closest shipwreck in the Belgian Part of the North Sea"

export_layer_as_s3(layer = shipwreck_dist,
                   layer_name = "distance to closest shipwreck",
                   layer_units = "m",
                   layer_description = "distance to closest shipwreck in the Belgian Part of the North Sea",
                   dir = s3_dir)

# s_y_m_4326 <- stars::st_as_stars(y_m_4326)
# names(s_y_m_4326) <- "Latitude"
# attr(s_y_m_4326, "units") <- "m"
# attr(s_y_m_4326, "description") <- "Latitude in EPSG:3035"

export_layer_as_s3(layer = y_m_4326,
                   layer_name = "Latitude",
                   layer_units = "m",
                   layer_description = "Latitude in EPSG:3035",
                   dir = s3_dir)

# s_x_m_4326 <- stars::st_as_stars(x_m_4326)
# names(s_x_m_4326) <- "Longitude"
# attr(s_x_m_4326, "units") <- "m"
# attr(s_x_m_4326, "description") <- "Longitude in EPSG:3035"

export_layer_as_s3(layer = x_m_4326,
                   layer_name = "Longitude",
                   layer_units = "m",
                   layer_description = "Longitude in EPSG:3035",
                   dir = s3_dir)

# s_lod_median_months <- stars::st_as_stars(lod_median_months) %>% suppressWarnings()
# names(s_lod_median_months) <- "median lod"
# attr(s_lod_median_months, "units") <- "h"
# attr(s_lod_median_months, "description") <- "median lod per month"

export_layer_as_s3(layer = lod_median_months,
                   layer_name = "median lod",
                   layer_units = "h",
                   layer_description = "median lod per month",
                   dir = s3_dir)

# s_sst_median_months <- stars::st_as_stars(sst_median_months) %>% suppressWarnings()
# names(s_sst_median_months) <- "median sst"
# attr(s_sst_median_months, "units") <- "Degree Celcius"
# attr(s_sst_median_months, "description") <- "median sst per month"

export_layer_as_s3(layer = sst_median_months,
                   layer_name = "median sst",
                   layer_units = "Degree Celcius",
                   layer_description = "median sst per month",
                   dir = s3_dir)

# s_n_active_tags_median_months <- stars::st_as_stars(n_active_tags_median_months) %>% suppressWarnings()
# names(s_n_active_tags_median_months) <- "median n_active_tags"
# attr(s_n_active_tags_median_months, "units") <- "count"
# attr(s_n_active_tags_median_months, "description") <- "median number of active acoustic transmitters per month"

export_layer_as_s3(layer = n_active_tags_median_months,
                   layer_name = "median n_active_tags",
                   layer_units = "count",
                   layer_description = "median number of active acoustic transmitters per month",
                   dir = s3_dir)

### DATA ###
# s_counts_median_months <- stars::st_as_stars(counts_median_months) %>% suppressWarnings()
# names(s_counts_median_months) <- "median counts"
# attr(s_counts_median_months, "units") <- "count"
# attr(s_counts_median_months, "description") <- "median counts of individuals per month"

export_layer_as_s3(layer = counts_median_months,
                   layer_name = "median counts",
                   layer_units = "count",
                   layer_description = "median counts of individuals per month",
                   dir = s3_dir)

### PREDICTIONS

# s_predictions_inside_owf_median_months <- stars::st_as_stars(predictions_inside_owf_median_months) %>% suppressWarnings()
# names(s_predictions_inside_owf_median_months) <- "median predicted counts"
# attr(s_predictions_inside_owf_median_months, "units") <- "count"
# attr(s_predictions_inside_owf_median_months, "description") <- "median predicted counts of individuals per month, data from inside owf"

export_layer_as_s3(layer = predictions_inside_owf_median_months,
                   layer_name = "median predicted counts",
                   layer_units = "count",
                   layer_description = "median predicted counts of individuals per month, data from inside owf",
                   dir = s3_dir)

# s_predictions_outside_owf_median_months <- stars::st_as_stars(predictions_outside_owf_median_months) %>% suppressWarnings()
# names(s_predictions_outside_owf_median_months) <- "median predicted counts"
# attr(s_predictions_outside_owf_median_months, "units") <- "count"
# attr(s_predictions_outside_owf_median_months, "description") <- "median predicted counts of individuals per month, data from outside owf"

export_layer_as_s3(layer = predictions_outside_owf_median_months,
                   layer_name = "median predicted counts",
                   layer_units = "count",
                   layer_description = "median predicted counts of individuals per month, data from outside owf",
                   dir = s3_dir)

# s_diff_owf <- stars::st_as_stars(diff_owf) %>% suppressWarnings()
# names(s_diff_owf) <- "difference in median predicted counts"
# attr(s_diff_owf, "units") <- "count"
# attr(s_diff_owf, "description") <- "difference median predicted counts of individuals per month, based on models trained with data from inside/outside owf"

export_layer_as_s3(layer = diff_owf,
                   layer_name = "difference in median predicted counts",
                   layer_units = "count",
                   layer_description = "difference median predicted counts of individuals per month, based on models trained with data from inside/outside owf",
                   dir = s3_dir)


## ----3. save list with all s3 obj links ------------------------------------------------------

s3_obj_list <- list.files(s3_dir, pattern = "\\.nc$", full.names = TRUE) %>%
  basename()

prefix <- paste0("https://", Sys.getenv("AWS_S3_ENDPOINT"))

s3_obj_list  %>%
  paste0(prefix, "/", .) %>%
  # tibble(filename = .) %>%
  readr::write_lines(file.path(s3_dir, "s3_links.csv"))
