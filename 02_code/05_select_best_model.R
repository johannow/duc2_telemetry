##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé & Lotte Pohl
# Email: johannes.nowe@vliz.be
# Date: 2025-10-28
# Script Name: ~/duc42_ga/02_code/05_select_best_model.R
# Script Description: select the best model among the trained models
# Script input: trained model objects (from file '04_train_GAM_model.R')
# Script output: best model object
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----load-packages------------------------------------------------------------

## ----load trained models into workspace ---------------------------------------
# the trained model .rds objects of chunk04 are in the folders of "./04_results/01_models'

files <- list.files(
  path = mod_dir,
  pattern = "\\.rds$",
  full.names = TRUE,
  recursive = TRUE
)

objects <- files %>%
  purrr::map(readRDS)

## ----compare models------------------------------------------------------------

# anovas, AIC, plot from mgcv package, DHARMa package?


## save final model object as output ---------------------------------------
# approach: save model foldername 
selected_model_name <- "m3_owf_elevation_sstlod_shipwreck_offset" #make this smarter, take from a list with model names e.g.
readr::write_lines(selected_model_name, file = file.path(mod_dir, "selected_model_name.csv"))
## give the model a general name that can be called from chunk07
