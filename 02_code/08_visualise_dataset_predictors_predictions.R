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
    count_median = median(count, na.rm = TRUE),
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
      `varnames<-`("count_median")
  })


# checks
plot(chunk01_monthly_rasters[[20]])

### 2.3 plot and save

data_monthly_path <- file.path(plot_dir, "data_monthly")
if (!dir.exists(data_monthly_path)) dir.create(data_monthly_path)

for (month_name in names(chunk01_monthly_rasters)) {
  r <- chunk01_monthly_rasters[[month_name]]
  
  png(paste0(data_monthly_path, "/median_individual_counts_", month_name, ".png"),
      width = 800, height = 600)
  plot(r, main = as.character(month_name))
  dev.off()
}


## old below
# now we match the name of the selected model to .rds files inside mod_dir
selected_model_path <- list.files(
  path = mod_dir,
  pattern = paste0("^", selected_model_name, "\\.rds$"),
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(selected_model_path) == 1) # sanity check

model <- readRDS(selected_model_path)



## extract model predictor names -------------------------------------------

predictor_names <- attr(terms(model), "term.labels")

# make predictions --------------------------------------------------------

predictions_owf_one <- list()
predictions_owf_zero <- list()
diff_owf <- list()
# make daily predictions
for(i in 1:nlyr(sst)){
  predictors_owf_one <- c(bathy, sst[[i]], lod[[i]], owf_one, shipwreck_dist, n_active_tags[[i]]) # owf raster with 1 everywhere
  predictors_owf_zero <- c(bathy, sst[[i]], lod[[i]], owf_zero, shipwreck_dist, n_active_tags[[i]]) # owf raster with 0 everywhere
  names(predictors_owf_one) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
  names(predictors_owf_zero) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
  
  # Predict
  predictions_owf_one[[i]] <- terra::predict(predictors_owf_one, model = model)
  predictions_owf_zero[[i]] <- terra::predict(predictors_owf_zero, model = model)
  
  #the difference in suitability when a owf everywhere vs nowhere
  diff_owf[[i]] <- predictions_owf_one[[i]] - predictions_owf_zero[[i]]
}
predictions_owf_one <- terra::rast(predictions_owf_one)
time(predictions_owf_one) <- dates
names(predictions_owf_one) <- dates %>% as.character()

predictions_owf_zero <- terra::rast(predictions_owf_zero)
time(predictions_owf_zero) <- dates
names(predictions_owf_zero) <- dates %>% as.character()

diff_owf <- terra::rast(diff_owf)
time(diff_owf) <- dates
names(diff_owf) <- dates %>% as.character()

terra::plot(diff_owf)


## make predictions for the owf_dist raster --------------------------------
### in addition to predicting counts of fish for rasters with 0 or 1 owf, we predict for the owf_dist raster layer
predictions_owf_dist <- list()
# make daily predictions
for(i in 1:nlyr(sst)){
  predictors_owf_dist <- c(bathy, sst[[i]], lod[[i]], owf_dist, shipwreck_dist, n_active_tags[[i]]) # owf raster with dist to owf
  names(predictors_owf_dist) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
  
  # Predict
  predictions_owf_dist[[i]] <- terra::predict(predictors_owf_dist, model = model)
}
predictions_owf_dist <- terra::rast(predictions_owf_dist)
time(predictions_owf_dist) <- dates
names(predictions_owf_dist) <- dates %>% as.character()

# write prediction files -------------------------------------------------------------
terra::writeCDF(x = predictions_owf_one,
                filename = file.path(pred_dir,"predictions_owf_one.nc"),
                varname = "predicted count",
                overwrite = TRUE)
terra::writeCDF(x = predictions_owf_zero,
                filename = file.path(pred_dir,"predictions_owf_zero.nc"),
                varname = "predicted count",
                overwrite = TRUE)
terra::writeCDF(x = diff_owf,
                filename = file.path(pred_dir,"diff_owf.nc"),
                varname = "Difference in predicted count",
                overwrite = TRUE)
terra::writeCDF(x = predictions_owf_dist,
                filename = file.path(pred_dir,"predictions_owf_dist.nc"),
                varname = "predicted count",
                overwrite = TRUE)

# make monthly prediction summaries and save as .png file ---------------------------------------
## potentially transfer to chunk08

owf_zero_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_owf_zero,
                        model_info = selected_model_name)

owf_one_monthly_median <- predictions_owf_one
aggregate_save_raster(raster_obj = ,
                      model_info = selected_model_name)

owf_dist_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_owf_dist,
                        model_info = selected_model_name)

owf_diff_monthly_median <- 
  aggregate_save_raster(raster_obj = diff_owf,
                        model_info = selected_model_name)


## aggregate predictors 

sst_monthly_median <- 
  aggregate_save_raster(raster_obj = sst,
                        model_info = "",
                        dir = processed_dir)

lod_monthly_median <- 
  aggregate_save_raster(raster_obj = lod,
                        model_info = "",
                        dir = processed_dir)


