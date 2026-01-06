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
OWF_dist <- terra::rast(file.path(processed_dir, "OWF_dist_rast.nc"))
shipwreck_dist <- terra::rast(file.path(processed_dir, "shipwreck_dist_rast.nc"))
sst <- terra::rast(file.path(processed_dir, "sst_rast.nc"))
n_active_tags <- terra::rast(file.path(processed_dir, "n_active_tags_rast.nc"))


##----resample relevant raster layers------------------------------------------------------------------
bathy <- terra::resample(bathy, sst) #need to align to allow predictions

## make 0 and 1 OWF raster layer ----------------------------------------------
owf_zero <- OWF_dist  
values(owf_zero) <- 0
owf_zero <- terra::mask(owf_zero, OWF_dist) #Now we have a raster with all distance to owf = 0
owf_one <- OWF_dist
values(owf_one) <- 1
owf_one <- terra::mask(owf_one, OWF_dist) #Now we have a raster with all distance to owf = 0

## extract model predictor names -------------------------------------------

predictor_names <- attr(terms(model), "term.labels")

# make predictions --------------------------------------------------------

predictions_owf_one <- list()
predictions_owf_zero <- list()
diff_owf <- list()
  # make daily predictions
  for(i in 1:nlyr(sst)){
    predictors_owf_one <- c(bathy, sst[[i]], lod[[i]], owf_one, shipwreck_dist, n_active_tags[[i]])
    predictors_owf_zero <- c(bathy, sst[[i]], lod[[i]], owf_zero, shipwreck_dist, n_active_tags[[i]]) #use a raster with distance to OWF = 0 to mimic all OWF
    names(predictors_owf_one) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
    names(predictors_owf_zero) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
  
    # Predict
    predictions_owf_one[[i]] <- terra::predict(predictors_owf_one, model = model)
    predictions_owf_zero[[i]] <- terra::predict(predictors_owf_zero, model = model)
    
    #the difference in suitability when a owf everywhere
    diff_owf[[i]] <- predictions_owf_one[[i]] - predictions_owf_zero[[i]]
    #if positive it means the suitability improved when an owf was placed on the spot
  }
  predictions <- terra::rast(predictions)
  predictions_all_owf <- terra::rast(predictions_all_owf)
  diff_owf <- terra::rast(diff_owf)
  
terra::plot(diff_owf)

terra::writeCDF(x = predictions,
                filename = file.path(pred_dir,"predictions_current.nc"),
                varname = "Encounter rate",
                overwrite = TRUE)
terra::writeCDF(x = predictions_all_owf,
                filename = file.path(pred_dir,"predictions_all_owf.nc"),
                varname = "Encounter rate",
                overwrite = TRUE)
terra::writeCDF(x = diff_owf,
                filename = file.path(pred_dir,"diff_owf.nc"),
                varname = "Difference in encounter rate",
                overwrite = TRUE)


# save some plots as documentation ----------------------------------------

save_plots <- TRUE

if(save_plots){
  # predictors
  png(file.path(pred_dir, "predictors.png"), width = 2000, height = 2000, res = 300)
  plot(predictors)
  dev.off()
  
  # predictors with the OWF layer being 1 everywhere
}

