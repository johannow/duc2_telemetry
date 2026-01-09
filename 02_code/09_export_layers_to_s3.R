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

## ----load-functions----------------------------------------------------------------
list.files(func_dir, pattern = "\\.R$", full.names = TRUE) |>
  purrr::walk(source)

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

## ----2. convert raster layers into stars obj -------------------------------------------------------------------------

### PREDICTORS ####
s_bathy <- stars::st_as_stars(bathy)
names(s_bathy) <- "elevation"
attr(s_bathy, "units") <- "m"
attr(s_bathy, "description") <- "bathymetry of the Belgian Part of the North Sea"

s_habitats <- stars::st_as_stars(habitats)
names(s_habitats) <- "habitat category"
attr(s_habitats, "units") <- ""
attr(s_habitats, "description") <- "seabed habitat categories of the Belgian Part of the North Sea"

s_owf_dist <- stars::st_as_stars(owf_dist)
names(s_owf_dist) <- "distance to closest offshore wind farm"
attr(s_owf_dist, "units") <- "m"
attr(s_owf_dist, "description") <- "distance to closest owf in the Belgian Part of the North Sea"

s_shipwreck_dist <- stars::st_as_stars(shipwreck_dist)
names(s_shipwreck_dist) <- "distance to closest shipwreck"
attr(s_shipwreck_dist, "units") <- "m"
attr(s_shipwreck_dist, "description") <- "distance to closest shipwreck in the Belgian Part of the North Sea"

s_y_m_4326 <- stars::st_as_stars(y_m_4326)
names(s_y_m_4326) <- "Latitude"
attr(s_y_m_4326, "units") <- "m"
attr(s_y_m_4326, "description") <- "Latitude in EPSG:3035"

s_x_m_4326 <- stars::st_as_stars(x_m_4326)
names(s_x_m_4326) <- "Longitude"
attr(s_x_m_4326, "units") <- "m"
attr(s_x_m_4326, "description") <- "Longitude in EPSG:3035"

s_lod_median_months <- stars::st_as_stars(lod_median_months) %>% suppressWarnings()
names(s_lod_median_months) <- "median lod"
attr(s_lod_median_months, "units") <- "h"
attr(s_lod_median_months, "description") <- "median lod per month"

s_sst_median_months <- stars::st_as_stars(sst_median_months) %>% suppressWarnings()
names(s_sst_median_months) <- "median sst"
attr(s_sst_median_months, "units") <- "Degree Celcius"
attr(s_sst_median_months, "description") <- "median sst per month"

s_n_active_tags_median_months <- stars::st_as_stars(n_active_tags_median_months) %>% suppressWarnings()
names(s_n_active_tags_median_months) <- "median n_active_tags"
attr(s_n_active_tags_median_months, "units") <- "count"
attr(s_n_active_tags_median_months, "description") <- "median number of active acoustic transmitters per month"

### DATA ###
s_counts_median_months <- stars::st_as_stars(counts_median_months) %>% suppressWarnings()
names(s_counts_median_months) <- "median counts"
attr(s_counts_median_months, "units") <- "count"
attr(s_counts_median_months, "description") <- "median counts of individuals per month"

### PREDICTIONS

s_predictions_inside_owf_median_months <- stars::st_as_stars(predictions_inside_owf_median_months) %>% suppressWarnings()
names(s_predictions_inside_owf_median_months) <- "median predicted counts"
attr(s_predictions_inside_owf_median_months, "units") <- "count"
attr(s_predictions_inside_owf_median_months, "description") <- "median predicted counts of individuals per month, data from inside owf"

s_predictions_outside_owf_median_months <- stars::st_as_stars(predictions_outside_owf_median_months) %>% suppressWarnings()
names(s_predictions_outside_owf_median_months) <- "median predicted counts"
attr(s_predictions_outside_owf_median_months, "units") <- "count"
attr(s_predictions_outside_owf_median_months, "description") <- "median predicted counts of individuals per month, data from outside owf"

s_diff_owf <- stars::st_as_stars(diff_owf) %>% suppressWarnings()
names(s_diff_owf) <- "difference in median predicted counts"
attr(s_diff_owf, "units") <- "count"
attr(s_diff_owf, "description") <- "difference median predicted counts of individuals per month, based on models trained with data from inside/outside owf"


## ----3. save stars obj as netCDF -------------------------------------------------------------------------

nc_file <- file.path(processed_dir, "test_s3.nc")
write_stars(raster, nc_file)  #, options = c("COMPRESS=4") # optional compression

# STOPPED HERE 20260109, 12:43pm. TODO: write function save_as_s3() that combines all steps (making the stars obj, saving it, and exporting to s3)

# Create raster
values <- sample(0:10, size = 100, replace = TRUE)

r <- rast(
  nrows = 10,
  ncols = 10,
  xmin = 0,
  xmax = 10,
  ymin = 50,
  ymax = 60,
  crs = "EPSG:4326"
)
values(r) <- values

# Convert to stars object
raster <- stars::st_as_stars(r)
names(raster) <- "random_raster"
attr(raster, "units") <- "arbitrary"
attr(raster, "description") <- "Random 10x10 raster covering 0–10E, 50–60N"

# ---- Save as NetCDF ----
nc_file <- file.path(processed_dir, "test_s3.nc")
write_stars(raster, nc_file)  #, options = c("COMPRESS=4") # optional compression

# ---- Configure MinIO/S3 ----
# save the credentials from edito (accessible via https://datalab.dive.edito.eu/account/storage) in an .Renviron file inside 
# repository's main directory ("~/duc2_telemetry/.Renviron")
# Note that these credentials are valid for 24h at a time (the .Renviron content needs to be renewed every 24h). 
# One you updated the .Renviron file contents, make sure to restart R. Otherwise, the .Renviron update won't come into effect
# Sys.setenv(
#   AWS_ACCESS_KEY_ID = "your info",
#   AWS_SECRET_ACCESS_KEY = "your info",
#   AWS_DEFAULT_REGION = "waw3-1",
#   AWS_SESSION_TOKEN = "your info",
#   AWS_S3_ENDPOINT = "minio.dive.edito.eu"
# )

# Initialize S3 client
minio <- paws::s3(
  config = list(
    credentials = list(
      creds = list(
        access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
        secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
        session_token = Sys.getenv("AWS_SESSION_TOKEN")
      )
    ),
    endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
    region = Sys.getenv("AWS_DEFAULT_REGION")
  )
)

# ---- Upload NetCDF to S3 ----
bucket_name <- Sys.getenv("bucket_name")  # Replace with your bucket
object_key <- "test_s3.nc"   # Name in S3

minio$put_object(
  Bucket = bucket_name,
  Key = object_key,
  Body = nc_file
)

# Verify
print(paste("Uploaded", nc_file, "to bucket", bucket_name))
