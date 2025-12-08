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
# the trained model .rds objects of chunk04 are in "./05_results/01_models'

files <- list.files(
  path = "./05_results/01_models",
  pattern = "\\.rds$",
  full.names = TRUE
)

objects <- files %>%
  purrr::map(readRDS)

## ----compare models------------------------------------------------------------

# anovas, AIC, plot from mgcv package, DHARMa package?