# edbuild_map_data.r
# 2023-07-11

# load ---------

options(scipen = 999)

library(tidyverse)
library(edbuildmapr)
library(sf)

sd_map_raw <- sd_shapepull(data_year = "2019")

# clean -----------

# create df of Minnesota district shapes
mn_sd_map <- sd_map_raw |> 
  # tidy colnames
  rename_with(tolower) |> 
  # only keep mn districts
  filter(state == "Minnesota") |> 
  # keep relevant columns for analysis
  select(geoid, geometry)
