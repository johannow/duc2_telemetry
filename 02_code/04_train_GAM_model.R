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
library(tibble)
library(gratia)
# library(rsample)
library(sf)
#library(purrr)

## ----load-functions----------------------------------------------------------------
list.files(func_dir, pattern = "\\.R$", full.names = TRUE) |>
  purrr::walk(source)

## ----load-data----------------------------------------------------------------
chunk03 <- readRDS(file.path(processed_dir, "output_chunk03.rds")) 

# make quick subset for faster model testing
chunk03_subset <- 
  chunk03 |>
    dplyr::slice_sample(n = (nrow(chunk03)/3) |> round())

## ----MODEL 1------------------------------------------------------------

# Step1: formulate the model
m1_formula <- acoustic_detection ~
                s(min_dist_owf, k = 20, bs = "tp") +
                s(elevation, k = 10, bs = "tp") +
                te(sst, lod, k = c(10,10), bs = c("tp", "cc")) +
                s(min_dist_shipwreck, k = 20, bs = "tp") #+
                #offset(n_active_tags)
# Step 2: train the model
m1 <- train_gam(formula = m1_formula,
                family = mgcv::nb(),
                dataset = chunk03_subset,
                dir = mod_dir,
                model_name = "m1_owf_elevation_sstlod_shipwreck_subset")

# Step 3: inspect and check the model's smooth terms

## TODO: maybe have gam.check plots render in another function to be shown in console
# m1 |> gam.check()
m1 |> check_and_save_gam(dir = mod_dir,
                         model_name = "m1_owf_elevation_sstlod_shipwreck_subset")

# Step 3b) check model's concurvity and Effective Degrees of Freedom (EDF)
m1_structure <- m1 |> check_gam_structure()

# Step 3c) check residuals
m1_residuals <- 
  m1 |> check_gam_residuals(data = chunk03_subset, 
                                          dir = mod_dir,
                                          model_name = "m1_owf_elevation_sstlod_shipwreck_subset")

which(m1_residuals$sim$scaledResiduals > 0.9)

# TODO: write and refine function to vosualise with gratia



# Step 4: visualise the model (using the gratia package)
m1 |> visualise_gam()


# Step 5: inspect the model residuals and distribution (using the DHARMa package)



# write a function that tests the model (dharma) and that visualises the model (gratia)


## ----MODEL 2------------------------------------------------------------

model2_formula <- acoustic_detection ~
  s(min_dist_owf, k = 20, bs = "tp") +
  # s(elevation, k = 10, bs = "tp") +
  te(sst, lod, k = c(10,10), bs = c("tp", "cc")) +
  # s(min_dist_shipwreck, k = 20, bs = "tp") +
  offset(n_active_tags)

model2 <- train_gam(formula = m2_formula,
                dataset = chunk03,
                dir = mod_dir,
                model_name = "m2_owf_sstlod_offset")


# save metadata of model formula
model2_formula |>
  deparse1(collapse = "") |>
  as_tibble() |>
  readr::write_csv(file = file.path(mod_dir, "model2_formula.csv"))

# # train model
# start_time <- Sys.time()
# model1 <- gam(m1_formula,
#                         family = "nb",
#                         method = "REML",
#                         data = chunk03)
# end_time <- Sys.time()
# print(end_time - start_time) # +- 1min

# bundle model
mod_bundle <- bundle::bundle(model2)

# save bundled model to .rds
saveRDS(mod_bundle, file.path(mod_dir, "model2.rds"))

# export model statistics
export_model_stats(model2, model_name = "model2", dir = mod_dir)

## ----MODEL 2 - scaled variables-----------------------------------------------

model2_formula_scaled <- acoustic_detection ~
  s(min_dist_owf_scaled, k = 20, bs = "tp") +
  s(elevation_scaled, k = 10, bs = "tp") +
  te(sst_scaled, lod_scaled, k = c(10,10), bs = c("tp", "cc")) +
  s(min_dist_shipwreck_scaled, k = 20, bs = "tp")

model2_scaled <- train_gam(formula = model2_formula_scaled,
                    dataset = chunk03,
                    dir = mod_dir,
                    model_name = "m2_owf_sstlod_offset_scaled")

# bundle model
mod_bundle <- bundle::bundle(model2_scaled)

# save bundled model to .rds
saveRDS(mod_bundle, file.path(mod_dir, "model2_scaled.rds"))

# export model statistics
export_model_stats(model2_scaled, model_name = "model2_scaled", dir = mod_dir)
## ----MODEL 3------------------------------------------------------------

m3_formula <- acoustic_detection ~
  s(min_dist_owf, k = 20, bs = "tp") +
  te(sst, lod, k = c(10,10), bs = c("tp", "cc")) +
  s(min_dist_shipwreck, k = 20, bs = "tp")

# save metadata of model formula
m3_formula |>
  deparse1(collapse = "") |>
  as_tibble() |>
  readr::write_csv(file = file.path(mod_dir, "model3_formula.csv"))

# train model
start_time <- Sys.time()
model3 <- gam(m3_formula,
              family = "nb",
              method = "REML",
              data = chunk03)
end_time <- Sys.time()
print(end_time - start_time) # +- 1min

# bundle model
mod_bundle <- bundle::bundle(model3)

# save bundled model to .rds
saveRDS(mod_bundle, file.path(mod_dir, "model3.rds"))

# export model statistics
export_model_stats(model3, model_name = "model3", dir = mod_dir)

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


