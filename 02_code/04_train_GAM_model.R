##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé
# Email: johannes.nowe@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/04_train_GAM_model.R
# Script Description: train a GAM model based on all the data
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----load-packages------------------------------------------------------------

library(dplyr)
library(geosphere)
library(mgcv)
library(lubridate)
library(bundle)
# library(rsample)
library(sf)
library(purrr)

## ----load-functions----------------------------------------------------------------
source(file.path(func_dir, "get_model_name.R"))
source(file.path(func_dir, "export_model_stats.R"))

## ----load-data----------------------------------------------------------------
chunk03 <- readRDS(file.path(processed_dir, "output_chunk03.rds")) 

## ----MODEL 1------------------------------------------------------------

m1_formula <- acoustic_detection ~
                s(min_dist_owf, k = 20, bs = "tp") +
                s(elevation, k = 10, bs = "tp") +
                te(sst, lod, k = c(10,10), bs = c("tp", "cc")) +
                s(min_dist_shipwreck, k = 20, bs = "tp")

# save metadata of model formula
m1_formula |>
  deparse1(collapse = "") |>
  as_tibble() |>
  readr::write_csv(file = file.path(mod_dir, "model1_formula.csv"))

# train model
start_time <- Sys.time()
model1 <- gam(m1_formula,
                        family = "nb",
                        method = "REML",
                        data = chunk03)
end_time <- Sys.time()
print(end_time - start_time) # +- 1min

# bundle model
mod_bundle <- bundle::bundle(model1)

# save bundled model to .rds
saveRDS(mod_bundle, file.path(mod_dir, "model1.rds"))

# export model statistics
export_model_stats(model1, model_name = "model1", dir = mod_dir)

## ----MODEL 2 - scaled variables-----------------------------------------------

m2_formula <- acoustic_detection ~
  s(min_dist_owf_scaled, k = 20, bs = "tp") +
  s(elevation_scaled, k = 10, bs = "tp") +
  te(sst_scaled, lod_scaled, k = c(10,10), bs = c("tp", "cc")) +
  s(min_dist_shipwreck_scaled, k = 20, bs = "tp")

start_time <- Sys.time()
# +- 1min
model2 <- gam(m2_formula,
              family = "nb",
              method = "REML",
              data = chunk03)
end_time <- Sys.time()
print(end_time - start_time)

# bundle model
mod2_bundle <- bundle::bundle(model2)

# get name for the model
model2_name <- get_model_name(chunk03, m2_formula)

# save bundled model to .rds
saveRDS(mod_bundle, file.path(mod_dir, model2_name))

# export model statistics
export_model_stats(model2, model_name = model2_name, dir = mod_dir)

#5min
# summary(gam_fitted_model)
# plot(gam_fitted_model,)
# gam_fitted_model <- gam(acoustic_detection ~
#                           s(min_dist_owf, k = 20, bs = "tp") +
#                           s(elevation, k = 10, bs = "tp") +
#                           s(sst, k = 10, bs = "tp")+
#                           s(lod, k = 10, bs = "cc") +
#                           s(min_dist_shipwreck, k = 20, bs = "tp"),
#                         family = "binomial",
#                         method = "REML",
#                         data = chunk03)
# 
start_time <- Sys.time()
# +- min
gam_fitted_model <- gam(acoustic_detection ~
                          s(min_dist_owf_scaled, k = 20, bs = "tp") +
                          s(elevation_scaled, k = 10, bs = "tp") +
                          s(sst_scaled, k = 10, bs = "tp")+
                          s(lod_scaled, k = 10, bs = "cc") +
                          s(min_dist_shipwreck, k = 20, bs = "tp"),
                        family = "nb",
                        method = "REML",
                        data = chunk03)
end_time <- Sys.time()
print(end_time - start_time)
start_time <- Sys.time()
# # +- min
# gam_fitted_model <- gam(acoustic_detection ~
#                           s(min_dist_owf_scaled, k = 20, bs = "tp") +
#                           habitat +
#                           s(sst_scaled, k = 10, bs = "tp")+
#                           s(lod_scaled, k = 10, bs = "cc") +
#                           s(min_dist_shipwreck_scaled, k = 20, bs = "tp"),
#                         family = "nb",
#                         method = "REML",
#                         data = chunk03)
# end_time <- Sys.time()
# print(end_time - start_time)
## ----save-results-------------------------------------------------------------
# bundle and then save
mod_bundle <- bundle::bundle(gam_fitted_model)
saveRDS(mod_bundle, file.path(mod_dir, "gam_fitted_model.rds"))


