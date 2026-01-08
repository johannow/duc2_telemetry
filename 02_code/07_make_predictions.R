##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé & Lotte Pohl
# Email: johannes.nowe@vliz.be
# Date: 2026-01-06
# Script Name: ~/duc42_ga/02_code/07_make_predictions.R
# Script Description: predict on the environmental rasters using the model selected in chunk05
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

## ----load model for inside OWF-------------------------------------------------------------------------
# the name of the selected model from chunk05 is saved into 'selected_model_name.csv' inside mod_dir
selected_model_name1 <- readLines(file.path(mod_dir, "selected_model_name.csv"))

# now we match the name of the selected model to .rds files inside mod_dir
selected_model_path1 <- list.files(
  path = mod_dir,
  pattern = paste0("^", selected_model_name1, "\\.rds$"),
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(selected_model_path1) == 1) # sanity check

model_insideOWF <- readRDS(selected_model_path1)

## ----load model for outside OWF-------------------------------------------------------------------------
# the name of the selected model from chunk05 is saved into 'selected_model_name.csv' inside mod_dir
selected_model_name2 <- readLines(file.path(mod_dir, "selected_model_name2.csv"))

# now we match the name of the selected model to .rds files inside mod_dir
selected_model_path2 <- list.files(
  path = mod_dir,
  pattern = paste0("^", selected_model_name2, "\\.rds$"),
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(selected_model_path2) == 1) # sanity check

model_outsideOWF <- readRDS(selected_model_path2)

## ----load prediction rasters-------------------------------------------------------------------------
bathy <- terra::rast(file.path(processed_dir, "bathy_rast.nc"))
habitats <- terra::rast(file.path(processed_dir, "habitats_rast.tif"))
lat <- terra::rast(file.path(processed_dir, "lat_rast.nc"))
lon <- terra::rast(file.path(processed_dir, "lon_rast.nc"))
x_m <- lon
values(x_m) <- values(init(project(lon, "EPSG:3035"), "x"))
# x_m <- project(lon, "EPSG:3035") # transform to m
# crs(x_m)
# res(x_m)  # now in meters
y_m <- project(lat, "EPSG:3035") # transform to m

lod <- terra::rast(file.path(processed_dir, "lod_rast.nc"))
time(lod) <- dates
owf_dist <- terra::rast(file.path(processed_dir, "OWF_dist_rast.nc"))
shipwreck_dist <- terra::rast(file.path(processed_dir, "shipwreck_dist_rast.nc"))
sst <- terra::rast(file.path(processed_dir, "sst_rast.nc"))
time(sst) <- dates
n_active_tags <- terra::rast(file.path(processed_dir, "n_active_tags_rast.nc"))
time(n_active_tags) <- dates

### make y_m and x_m rasters
chunk03 <- readRDS(file.path(processed_dir, "output_chunk03.rds"))


##----resample relevant raster layers------------------------------------------------------------------
bathy <- terra::resample(bathy, sst) #need to align to allow predictions

# 1. reference raster in EPSG:4326
r_4326 <- bathy

# 2. project grid to EPSG:3035
r_3035 <- project(r_4326, "EPSG:3035")

# 3. create projected coordinates (meters)
x_m_3035 <- init(r_3035, "x")
y_m_3035 <- init(r_3035, "y")

test_y <- bathy %>% project("EPSG:3035") %>% init("y") %>% mask(shipwreck_dist)
test_x <- bathy %>% project("EPSG:3035") %>% init("x") %>% mask(shipwreck_dist)

# 4. project values back to 4326 grid
x_m_4326 <- project(x_m_3035, r_4326, method = "near") %>% mask(shipwreck_dist)
y_m_4326 <- project(y_m_3035, r_4326, method = "near") %>% mask(shipwreck_dist)

ext(sst[[1]]) == ext(y_m_4326)

# x_m <- rasterize(
#   vect(chunk03),
#   bathy,
#   field = "x_m",
#   fun = mean
# )
# varnames(x_m) <- "x_m"
# 
# y_m <- rasterize(
#   vect(chunk03),
#   bathy,
#   field = "y_m",
#   fun = mean
# )
# varnames(y_m) <- "y_m"

# x_m <- terra::resample(x_m, sst)
# test <- project(x_m, bathy)

## make 0 and 1 OWF raster layer ----------------------------------------------
owf_zero <- owf_dist  
values(owf_zero) <- 0
owf_zero <- terra::mask(owf_zero, owf_dist) #Now we have a raster with all distance to owf = 0
owf_one <- owf_dist
values(owf_one) <- 1
owf_one <- terra::mask(owf_one, owf_dist) #Now we have a raster with all distance to owf = 0

## extract model predictor names -------------------------------------------

predictor_names <- attr(terms(model_outsideOWF), "term.labels")

# make predictions --------------------------------------------------------

## model inside OWF
predictions_inside_owf <- list()

# make daily predictions
for(i in 1:nlyr(sst)){
  predictors <- c(bathy, sst[[i]], lod[[i]], shipwreck_dist, y_m_4326, x_m_4326, n_active_tags[[i]])
  names(predictors) <- c("elevation", "sst", "lod", "min_dist_shipwreck", "y_m", "x_m", "n_active_tags")
 
  # Predict
  predictions_inside_owf[[i]] <- terra::predict(predictors, model = model_insideOWF)
}

predictions_inside_owf <- terra::rast(predictions_inside_owf)
time(predictions_inside_owf) <- dates
names(predictions_inside_owf) <- dates %>% as.character()

## model outside OWF
predictions_outside_owf <- list()

# make daily predictions
for(i in 1:nlyr(sst)){
  predictors <- c(bathy, sst[[i]], lod[[i]], shipwreck_dist, y_m_4326, x_m_4326, n_active_tags[[i]])
  names(predictors) <- c("elevation", "sst", "lod", "min_dist_shipwreck", "y_m", "x_m", "n_active_tags")
 
  # Predict
  predictions_outside_owf[[i]] <- terra::predict(predictors, model = model_outsideOWF)
}

predictions_outside_owf <- terra::rast(predictions_outside_owf)
time(predictions_outside_owf) <- dates
names(predictions_outside_owf) <- dates %>% as.character()


# old: with layers of OWF == 1 and OWF == 0
# predictions_owf_one <- list()
# predictions_owf_zero <- list()
# diff_owf <- list()
#   # make daily predictions
#   for(i in 1:nlyr(sst)){
#     predictors_owf_one <- c(bathy, sst[[i]], lod[[i]], owf_one, shipwreck_dist, n_active_tags[[i]]) # owf raster with 1 everywhere
#     predictors_owf_zero <- c(bathy, sst[[i]], lod[[i]], owf_zero, shipwreck_dist, n_active_tags[[i]]) # owf raster with 0 everywhere
#     names(predictors_owf_one) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
#     names(predictors_owf_zero) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
# 
#     # Predict
#     predictions_owf_one[[i]] <- terra::predict(predictors_owf_one, model = model)
#     predictions_owf_zero[[i]] <- terra::predict(predictors_owf_zero, model = model)
# 
#     #the difference in suitability when a owf everywhere vs nowhere
#     diff_owf[[i]] <- predictions_owf_one[[i]] - predictions_owf_zero[[i]]
#   }
#   predictions_owf_one <- terra::rast(predictions_owf_one)
#   time(predictions_owf_one) <- dates
#   names(predictions_owf_one) <- dates %>% as.character()
# 
#   predictions_owf_zero <- terra::rast(predictions_owf_zero)
#   time(predictions_owf_zero) <- dates
#   names(predictions_owf_zero) <- dates %>% as.character()
# 
#   diff_owf <- terra::rast(diff_owf)
#   time(diff_owf) <- dates
#   names(diff_owf) <- dates %>% as.character()
# 
# terra::plot(diff_owf)
# 
# 
# ## make predictions for the owf_dist raster --------------------------------
# ### in addition to predicting counts of fish for rasters with 0 or 1 owf, we predict for the owf_dist raster layer
# predictions_owf_dist <- list()
# # make daily predictions
# for(i in 1:nlyr(sst)){
#   predictors_owf_dist <- c(bathy, sst[[i]], lod[[i]], owf_dist, shipwreck_dist, n_active_tags[[i]]) # owf raster with dist to owf
#   names(predictors_owf_dist) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck", "n_active_tags")
#   
#   # Predict
#   predictions_owf_dist[[i]] <- terra::predict(predictors_owf_dist, model = model)
# }
# predictions_owf_dist <- terra::rast(predictions_owf_dist)
# time(predictions_owf_dist) <- dates
# names(predictions_owf_dist) <- dates %>% as.character()

# write prediction files -------------------------------------------------------------
terra::writeCDF(x = predictions_inside_owf,
                filename = file.path(pred_dir,"predictions_inside_owf.nc"),
                varname = "predicted count",
                overwrite = TRUE)
terra::writeCDF(x = predictions_outside_owf,
                filename = file.path(pred_dir,"predictions_outside_owf.nc"),
                varname = "predicted count",
                overwrite = TRUE)
## aggregate per month and write file

inside_owf_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_inside_owf,
                        model_info = selected_model_name1,
                        varname = "monthly predicted count",
                        dir = processed_dir,
                        filename = "output_chunk07_insideOWF",
                        range = c(-100, 0))

outside_owf_monthly_median <- 
  aggregate_save_raster(raster_obj = predictions_outside_owf,
                        model_info = selected_model_name2,
                        varname = "monthly predicted count",
                        dir = processed_dir,
                        filename = "output_chunk07_outsideOWF",
                        range = c(-100, 0))


# calculate the difference in predictions from inside to outside ----------


# approach 2: calculate difference from the aggregated predictions
diff_owf_outside_inside <- inside_owf_monthly_median - outside_owf_monthly_median
plot(diff_owf_outside_inside)
# # old
# terra::writeCDF(x = predictions_owf_one,
#                 filename = file.path(pred_dir,"predictions_owf_one.nc"),
#                 varname = "predicted count",
#                 overwrite = TRUE)
# terra::writeCDF(x = predictions_owf_zero,
#                 filename = file.path(pred_dir,"predictions_owf_zero.nc"),
#                 varname = "predicted count",
#                 overwrite = TRUE)
# terra::writeCDF(x = diff_owf,
#                 filename = file.path(pred_dir,"diff_owf.nc"),
#                 varname = "Difference in predicted count",
#                 overwrite = TRUE)
# terra::writeCDF(x = predictions_owf_dist,
#                 filename = file.path(pred_dir,"predictions_owf_dist.nc"),
#                 varname = "predicted count",
#                 overwrite = TRUE)
