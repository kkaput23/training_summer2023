# edbuild_clean.R
# 2023-07-11

# load ---------

options(scipen = 999)

library(tidyverse)
library(edbuildr)

dist_geo_raw <- masterpull(data_year = "2019", data_type = "geo")

# clean -----------

# create a tidy edbuild df w/ only Minnesota districts
mn_dist_raw <- dist_geo_raw |> 
  # change colnames to lowercase
  rename_with(tolower) |> 
  # select mn districts
  filter(state == "Minnesota") 

# create df of outlier mn districts to exclude from analysis
mn_dist_na_schools <- mn_dist_raw |>
  # only keep districts that have no schools
  filter(is.na(doperational_schools))

# create df of mn districts for analysis
mn_dist <- mn_dist_raw |>
  # only keep districts that actually have schools
  filter(!is.na(doperational_schools)) |> 
  # create simplified urbanicity 
  mutate(urbanicity = fct_collapse(as.factor(durbanicity),
                                   City = c("11-City: Large", 
                                            "12-City: Mid-size",
                                            "13-City: Small"),
                                   Suburb = c("21-Suburb: Large",
                                              "22-Suburb: Mid-size",
                                              "23-Suburb: Small"),
                                   Town = c("31-Town: Fringe",
                                            "32-Town: Distant",
                                            "33-Town: Remote"),
                                   Rural = c("41-Rural: Fringe",
                                             "42-Rural: Distant",
                                             "43-Rural: Remote")))