##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-01-07
# Script Name: ~/duc42_ga/02_code/08_plot_predictions.R
# Script Description: plot monthly rasters of the dataset and the predictions
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----libraries-------------------------------------------------------------------------
library(bundle)
library(terra)
library(dplyr)
library(lubridate)
library(purrr)
library(rlang)

## ----load-functions----------------------------------------------------------------
list.files(func_dir, pattern = "\\.R$", full.names = TRUE) |>
  purrr::walk(source)

## ----parameters----------------------------------------------------------------
start <- "2021-01-01"
end <- "2022-12-31"
# Create sequence of dates
dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "day")

## ----1. load predictors-------------------------------------------------------------------------
bathy <- terra::rast(file.path(processed_dir, "bathy_rast.nc"))
habitats <- terra::rast(file.path(processed_dir, "habitats_rast.tif"))
lat <- terra::rast(file.path(processed_dir, "lat_rast.nc"))
lon <- terra::rast(file.path(processed_dir, "lon_rast.nc"))
lod <- terra::rast(file.path(processed_dir, "lod_rast.nc"))
# define the time for time-variant predictors layers
time(lod) <- dates
owf_dist <- terra::rast(file.path(processed_dir, "OWF_dist_rast.nc"))
shipwreck_dist <- terra::rast(file.path(processed_dir, "shipwreck_dist_rast.nc"))
sst <- terra::rast(file.path(processed_dir, "sst_rast.nc"))
time(sst) <- dates
n_active_tags <- terra::rast(file.path(processed_dir, "n_active_tags_rast.nc"))
time(n_active_tags) <- dates

### save predictor raster plots
predictors_plots_dir <- file.path(processed_dir, "predictors_plots")
if (!dir.exists(predictors_plots_dir)) dir.create(predictors_plots_dir)

#### temporally constant predictors

aggregate_save_raster(raster_obj = bathy,
                      aggregate = F,
                      varname = "elevation",
                      dir = predictors_plots_dir)

aggregate_save_raster(raster_obj = shipwreck_dist,
                      aggregate = F,
                      varname = "min_dist_to_shipwreck",
                      dir = predictors_plots_dir)

aggregate_save_raster(raster_obj = habitats,
                      aggregate = F,
                      varname = "seabed_habitat",
                      dir = predictors_plots_dir)

aggregate_save_raster(raster_obj = owf_dist,
                      aggregate = F,
                      varname = "min_dist_to_owf",
                      dir = predictors_plots_dir)


## aggregate time-variant predictors 
sst_monthly_median <- 
  aggregate_save_raster(raster_obj = sst,
                        varname = "sst",
                        aggregate = T,
                        dir = predictors_plots_dir)

lod_monthly_median <- 
  aggregate_save_raster(raster_obj = lod,
                        varname = "lod",
                        dir = predictors_plots_dir)

n_active_tags <- 
  aggregate_save_raster(raster_obj = n_active_tags,
                        varname = "n_active_tags",
                        dir = predictors_plots_dir)


## ----2. load dataset-------------------------------------------------------------------------
# data <- readRDS(file.path(processed_dir, "output_chunk03.rds"))
chunk01 <- readRDS(file.path(processed_dir, "output_chunk01.rds"))

### 2.1 summarise dataset per monthyear (using the median count of individuals)
chunk01_monthly <- chunk01 %>%
  dplyr::mutate(
    year_month = floor_date(time, unit = "month")
  ) %>%
  dplyr::group_by(year_month, station_name, geometry) %>%
  dplyr::summarise(
    "count_median" = median(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  st_as_sf()

### 2.2 rasterise

r_template <- rast(sst[[1]])
values(r_template) <- NA

chunk01_vect <- vect(chunk01_monthly)

chunk01_monthly_rasters <- chunk01_monthly %>%
  distinct(year_month) %>%
  pull(year_month) %>%
  set_names() %>%
  purrr::map(~ {
    v <- chunk01_vect[chunk01_vect$year_month == .x, ]
    
    rasterize(
      v,
      r_template,
      field = "count_median",
      fun = sum,
      background = NA
    ) %>%
      `varnames<-`("median count")
  })

# checks
plot(chunk01_monthly_rasters[[20]])

### 2.3 plot and save

data_monthly_dir <- file.path(aggregations_dir, "median_counts_monthly")
if (!dir.exists(data_monthly_dir)) dir.create(data_monthly_dir)

for (month_name in names(chunk01_monthly_rasters)) {
  r <- chunk01_monthly_rasters[[month_name]]
  
  png(paste0(data_monthly_dir, "/median_individual_counts_", month_name, ".png"),
      width = 800, height = 600)
  plot(r, main = as.character(month_name))
  dev.off()
}

### 4. write raster
# stack the list into one SpatRaster
chunk01_monthly_rasters_stack <- terra::rast(chunk01_monthly_rasters)

suppressWarnings( # suppress warning about there being no time dimension
terra::writeCDF(x = chunk01_monthly_rasters_stack,
                filename = file.path(data_monthly_dir,"median_counts_monthly.nc"),
                varname = "median count",
                overwrite = TRUE)
)


## ----3. load predictions -------------------------------------------------------------------------
predictions_inside_owf <- terra::rast(file.path(pred_dir, "predictions_inside_owf.nc"))
predictions_outside_owf <- terra::rast(file.path(pred_dir, "predictions_outside_owf.nc"))

# predictions_owf_one <- terra::rast(file.path(pred_dir, "predictions_owf_one.nc"))
# predictions_owf_zero <- terra::rast(file.path(pred_dir, "predictions_owf_zero.nc"))
# predictions_owf_dist <- terra::rast(file.path(pred_dir, "predictions_owf_dist.nc"))

### make monthly prediction summaries and save as .png file ---------------------------------------

## ----load model infos-------------------------------------------------------------------------
# the name of the selected model from chunk05 is saved into 'selected_model_name.csv' inside mod_dir
selected_model_name1 <- readLines(file.path(mod_dir, "selected_model_name.csv"))
selected_model_name2 <- readLines(file.path(mod_dir, "selected_model_name2.csv"))

predictions_monthly_dir <- file.path(aggregations_dir, "predictions_monthly")
if (!dir.exists(predictions_monthly_dir)) dir.create(predictions_monthly_dir)

inside_owf_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_inside_owf,
                        model_info = selected_model_name1,
                        varname = "monthly predicted count",
                        dir = predictions_monthly_dir)

outside_owf_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_outside_owf,
                        model_info = selected_model_name2,
                        varname = "monthly predicted count",
                        dir = predictions_monthly_dir)

# owf_zero_monthly_median <- 
#   aggregate_save_raster(raster_obj = predictions_owf_zero,
#                         model_info = selected_model_name,
#                         varname = "monthly predicted count",
#                         dir = predictions_monthly_dir)
# 
# owf_one_monthly_median <- 
#   aggregate_save_raster(raster_obj = predictions_owf_one,
#                       model_info = selected_model_name,
#                       varname = "monthly predicted count",
#                       dir = predictions_monthly_dir)
# 
# owf_dist_monthly_median <- 
#   aggregate_save_raster(raster_obj = predictions_owf_dist,
#                         model_info = selected_model_name,
#                         varname = "monthly predicted count",
#                         dir = predictions_monthly_dir)

# owf_diff_monthly_median <- 
#   aggregate_save_raster(raster_obj = diff_owf,
#                         model_info = selected_model_name)


