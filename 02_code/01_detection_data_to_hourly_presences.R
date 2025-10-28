##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/01_detection_data_to_hourly_presences.R
# Script Description: process raw acoustic detections and deployments data to a 
#                     monthly aggregation of presences per station
# SETUP ------------------------------------
 cat("\014")                          # Clears the console
 rm(list = ls())                      # Remove all variables of the work space
 source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----load-packages------------------------------------------------------------
# unsure still how .renv works with loading packages - 
#so for now loading them like this - change in the future

# install.packages(c("mregions2", "knitr", "dplyr", 
# "remotes", "leaflet", "sf", "ggplot2", "plotly"))
# remotes::install_github("inbo/etn@v2.3-beta", force = TRUE)

library(dplyr)
library(knitr)
library(tidyr)
library(lubridate)
library(sf)
# getwd()

## ----parameters----------------------------------------------------------------
start <- "2021-01-01"
end <- "2022-12-31"

## ----read raw data----------------------------------------------------------------

# Belgian EEZ --> will be the bounding box
BPNS <- 
  sf::st_read(file.path(raw_dir, "BPNS.gpkg")) 

### ----acoustic-detections--------------------------------------------------

detections_raw <- base::readRDS("01_data/01_raw_data/detections.rds") 

detections_days <- detections_raw %>%
  dplyr::select(time = date_time,
                animal_id,
                deploy_latitude,
                deploy_longitude,
                station_name)%>%
  dplyr::mutate(time = lubridate::date(time))%>%
  group_by(time, station_name)%>%
  distinct(animal_id)%>%
  summarise(count = n())
            
detections_month <- 
  detections_raw |>
      dplyr::mutate(
       month = lubridate::month(date_time))  |>
      dplyr::group_by(month, station_name) |>
      dplyr::summarise(n_dets = dplyr::n(), .groups = "drop",
      n_animals = n_distinct(animal_id) %>% as.integer(),
      lat = deploy_latitude |> mean(na.rm = T),
      lon = deploy_longitude |> mean(na.rm = T)) |>
  # make into sf object
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) 

# save the monthly detections dataframe
base::saveRDS(detections_month, file.path(processed_dir, "detections_month.rds"))

### ----animals--------------------------------------------------

animals <- base::readRDS("01_data/01_raw_data/animals.rds") 



tags <- base::readRDS("01_data/01_raw_data/tags.rds") 
# make a simple dataframe with start and end date of each tag

tags_start_end <-
  animals |> 
  dplyr::select(tag_serial_number, tag_date) |>
  dplyr::left_join(tags |> 
                     # only keep one row per tag_serial_number
                     dplyr::distinct(tag_serial_number, battery_estimated_end_date) ) |>
  dplyr::mutate(battery_estimated_end_date = as.Date(battery_estimated_end_date)) |>
  dplyr::rename(tag_start = tag_date,
                tag_end = battery_estimated_end_date)

### ----acoustic_deployments-------------------------------------------------

deployments <-
  base::readRDS(file.path(raw_dir, "deployments.rds"))

## ----create-time-index--------------------------------------------------------
# Create a time index for each day from start to end
time_index <- 
  tidyr::tibble(time = seq(
    from = as.Date(start),
    to = as.Date(end),
    by = "day"))

## ----stations-days------------------------------------------------------------
stations <- 
  deployments |>
    dplyr::group_by(station_name) |>
    dplyr::summarise(deploy_latitude = deploy_latitude |> mean(na.rm = T),
                     deploy_longitude = deploy_longitude |> mean(na.rm = T))
base::saveRDS(stations, file.path(processed_dir, "stations.rds"))
## ----deployments-days---------------------------------------------------------
deployments_days <-
  time_index |>
    dplyr::left_join(deployments |>
              dplyr::select(station_name, deploy_date_time, recover_date_time) |>
              sf::st_drop_geometry() |>
              dplyr::mutate(value_deploy = 0,
                            deploy_date = as.Date(deploy_date_time),
                            recover_date = as.Date(recover_date_time)),
    by = dplyr::join_by(between(time, deploy_date, recover_date)))|>
    dplyr::select(-c(deploy_date_time, recover_date_time, deploy_date, recover_date))
base::saveRDS(deployments_days, "01_data/02_processed_data/deployments_days.rds")


## ----merge-dataframes---------------------------------------------------------
detections_day <- left_join(deployments_days, detections_days, by = c("time", "station_name"))%>%
  mutate(across(count, ~ replace_na(.,0)))%>%
  dplyr::select(-value_deploy)
detections_day <- left_join(detections_day, active_tags, by = "time")
saveRDS(detections_day, file = "01_data/02_processed_data/detections_day.rds")

#If we use counts per day instead
output_chunk01 <- 
  detections_day |>
  dplyr::left_join(stations, by = join_by(station_name))
dplyr::left_join(active_tags, by = join_by(time))

## ----save-outputs-------------------------------------------------------------
# as RDS
base::saveRDS(output_chunk01, file.path(processed_dir, "output_chunk01.rds"))


