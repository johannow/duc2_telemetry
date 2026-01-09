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
# lat <- terra::rast(file.path(processed_dir, "lat_rast.nc"))
# lon <- terra::rast(file.path(processed_dir, "lon_rast.nc"))

# per-month aggregated layers of time-variant rasters are stored in the aggregations_dir
predictors_monthly_dir <- file.path(aggregations_dir, "predictors_monthly")

lod_median_months <- terra::rast(file.path(predictors_monthly_dir, "lod_median_months.nc"))
sst_median_months <- terra::rast(file.path(predictors_monthly_dir, "sst_median_months.nc"))
n_active_tags_median_months <- terra::rast(file.path(predictors_monthly_dir, "n_active_tags_median_months.nc"))

### ACOUSTIC TELEMETRY DATA ###
data_monthly_dir <- file.path(aggregations_dir, "median_counts_monthly")

### PREDICTIONS ###
predictions_monthly_dir <- file.path(aggregations_dir, "predictions_monthly")

predictions_inside_owf_median_months <- terra::rast(file.path(predictions_monthly_dir, "predictions_inside_owf_median_months.nc"))
predictions_outside_owf_median_months <- terra::rast(file.path(predictions_monthly_dir, "predictions_inside_owf_median_months.nc"))
predictions_inside_owf_median_months <- terra::rast(file.path(predictions_monthly_dir, "predictions_inside_owf_median_months.nc"))



## ----load model for inside OWF-------------------------------------------------------------------------

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
