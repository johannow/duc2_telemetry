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
library(rsample)
library(sf)
library(purrr)

## ----load-data----------------------------------------------------------------
chunk03 <- readRDS(file.path(processed_dir, "output_chunk03.rds")) #|>
  # for MULTIPOINT: extract first POINT
  # dplyr:: mutate(
  #   geometry = map(st_geometry(.), ~ 
  #                    if (inherits(.x, "MULTIPOINT")) {
  #                      st_point(.x[1, ])  # extract first point as sfg POINT
  #                    } else {
  #                      .x                  # leave POINT as-is
  #                    }) %>%
  #     st_sfc(crs = st_crs(chunk03)))

## ----lod-------------------------------------------------------------------------
#The formula for length of day is daylength(lat, day of year)
chunk03 <- chunk03 |>
  dplyr:: mutate(lod = geosphere::daylength(deploy_latitude, lubridate::yday(time)))

  # dplyr::mutate(lod = geosphere::daylength(deploy_latitude, lubridate::yday(time)))

## ----prep-model-df------------------------------------------------------------

# Convert target to factor (if not yet)
# chunk03_prep <- chunk03 |>
#   dplyr::mutate(
#     acoustic_detection = factor(acoustic_detection, levels = c(1, 0)), #tidymodels assume first level is level of interest
#     habitat = as.factor(habitat)) |>
#   sf::st_drop_geometry() |>
#   dplyr::select(c(acoustic_detection,
#                   min_dist_owf,
#                   min_dist_shipwreck,
#                   elevation,
#                   sst,
#                   lod)) # select the columns we use for the modelling

# EDIT LP 20251208: subset data df to reduce model runtime for now
split <- rsample::initial_split(chunk03, prop = 0.5)
train <- rsample::training(split)
test  <- rsample::testing(split)

## ----first model------------------------------------------------------------

start_time <- Sys.time()
# +- min
model1 <- gam(acoustic_detection ~
                          s(min_dist_owf, k = 20, bs = "tp") +
                          s(elevation, k = 10, bs = "tp") +
                          te(sst, lod, k = c(10,10), bs = c("tp", "cc")) +
                          s(min_dist_shipwreck, k = 20, bs = "tp"),
                        family = "nb",
                        method = "REML",
                        data = chunk03)
end_time <- Sys.time()
print(end_time - start_time)

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
# start_time <- Sys.time()
# # +- min
# gam_fitted_model <- gam(acoustic_detection ~
#                           s(min_dist_owf_scaled, k = 20, bs = "tp") +
#                           s(elevation_scaled, k = 10, bs = "tp") +
#                           s(sst_scaled, k = 10, bs = "tp")+
#                           s(lod_scaled, k = 10, bs = "cc") +
#                           s(min_dist_shipwreck_scaled, k = 20, bs = "tp"),
#                         family = "nb",
#                         method = "REML",
#                         data = chunk03)
# end_time <- Sys.time()
# print(end_time - start_time)
# start_time <- Sys.time()
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


