##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé & Lotte Pohl
# Email: johannes.nowe@vliz.be
# Date: 2026-01-06
# Script Name: ~/duc42_ga/02_code/06_make_predictions.R
# Script Description: predict on the environmental rasters using the model
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----libraries-------------------------------------------------------------------------
library(bundle)
library(terra)

## ----load-functions----------------------------------------------------------------
list.files(func_dir, pattern = "\\.R$", full.names = TRUE) |>
  purrr::walk(source)

## ----parameters----------------------------------------------------------------
start <- "2021-01-01"
end <- "2022-12-31"
# Create sequence of dates
dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "day")

## ----load model-------------------------------------------------------------------------
# the selected model from chunk05 will be saved as 'selected_model.rds'
model <- readRDS(file.path(mod_dir, "selected_model.rds"))
model <- unbundle(model)

## ----load prediction rasters-------------------------------------------------------------------------
bathy <- terra::rast(file.path(processed_dir, "bathy_rast.nc"))
habitats <- terra::rast(file.path(processed_dir, "habitats_rast.tif"))
lat <- terra::rast(file.path(processed_dir, "lat_rast.nc"))
lon <- terra::rast(file.path(processed_dir, "lon_rast.nc"))
lod <- terra::rast(file.path(processed_dir, "lod_rast.nc"))
time(lod) <- dates
owf_dist <- terra::rast(file.path(processed_dir, "OWF_dist_rast.nc"))
shipwreck_dist <- terra::rast(file.path(processed_dir, "shipwreck_dist_rast.nc"))
sst <- terra::rast(file.path(processed_dir, "sst_rast.nc"))
time(sst) <- dates
n_active_tags <- terra::rast(file.path(processed_dir, "n_active_tags_rast.nc"))
time(n_active_tags) <- dates


##----resample relevant raster layers------------------------------------------------------------------
bathy <- terra::resample(bathy, sst) #need to align to allow predictions

## make 0 and 1 OWF raster layer ----------------------------------------------
owf_zero <- owf_dist  
values(owf_zero) <- 0
owf_zero <- terra::mask(owf_zero, owf_dist) #Now we have a raster with all distance to owf = 0
owf_one <- owf_dist
values(owf_one) <- 1
owf_one <- terra::mask(owf_one, owf_dist) #Now we have a raster with all distance to owf = 0

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
diff_owf <- list()
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

# make monthly prediction summaries ---------------------------------------

owf_zero_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_owf_zero,
                        model_info = "model2_offset")

owf_one_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_owf_one,
                        model_info = "model2_offset")

owf_dist_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_owf_dist,
                        model_info = "model2_offset")

owf_diff_monthly_median <- 
  aggregate_save_raster(raster_obj = diff_owf,
                        model_info = "model2_offset")


## aggregate predictors

sst_monthly_median <- tapp(
  sst,
  index = "months",
  fun = median,
  na.rm = TRUE
)

sst_monthly_median <- 
  aggregate_save_raster(raster_obj = sst,
                        model_info = "",
                        dir = processed_dir)

lod_monthly_median <- 
  aggregate_save_raster(raster_obj = lod,
                        model_info = "",
                        dir = processed_dir)


# save some plots as documentation ----------------------------------------

# predictors
png(file.path(pred_dir, "predictors.png"), width = 2000, height = 2000, res = 300)
plot(predictors)
dev.off()

# predictors
png(file.path(pred_dir, "predictors.png"), width = 2000, height = 2000, res = 300)
plot(predictors)
dev.off()

# write monthly aggregated files ------------------------------------------
## TODO: save model metadata, or link which model gave which prediction

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

