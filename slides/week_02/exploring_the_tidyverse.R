# exploring_the_tidyverse.R
# 2023-06-13

# load ----------

options(scipen = 999)

library(tidyverse)
library(edbuildr)

# Krista likes to denote which spreadsheet is raw for transparency 
dist_fy19_raw <- masterpull(data_type = "geo")

# filter Minnesota data and clean -----

# filter, rename, and mutate data for Minnesota 
mn_ed_data <- dist_fy19_raw |>
  filter(State == "Minnesota") |>
  rename(district = NAME,
         county = County,
         enroll = ENROLL, 
         total_local_rev = LR,
         total_state_rev = SR,
         total_state_local_rev = SLR, 
         urbanicity = dUrbanicity, 
         operational_schools = dOperational_schools, 
         district_type = dType, 
         white_enroll = dWhite, 
         sped_enroll = dIEP, 
         ell_enroll = dLEP, 
         econ_dis_enroll = StPov,
         bipoc_pct = pctNonwhite, 
         pov_pct = StPovRate,
         median_house_income = MHI, 
         median_prop_value = MPV) |>
  mutate(bipoc_enroll = enroll - white_enroll,
         ell_pct = ell_enroll/enroll, 
         sped_pct = sped_enroll/enroll, 
         loc_rev_pp = total_local_rev/enroll,
         state_rev_pp = total_state_rev/enroll,
         local_state_rev_pp = total_state_local_rev/enroll) |>
  select(district, county, enroll, loc_rev_pp, state_rev_pp, local_state_rev_pp, 
         total_local_rev, total_state_rev, total_state_local_rev, urbanicity, 
         operational_schools, district_type, pov_pct, bipoc_pct, ell_pct, sped_pct)
  
  
# histograms ----------
# create histograms of prop val, mhi, and state + local pp rev

# histogram of median property value
ggplot(data = state_summary) +
  geom_histogram(mapping = aes(x = mpv_med), binwidth = 40000)

# histogram of median household income
ggplot(data = state_summary) +
  geom_histogram(mapping = aes(x = mhi_med), binwidth = 3000)

# histogram of state + local pp rev
ggplot(data = state_summary) +
  geom_histogram(mapping = aes(x = state_loc_rev_pp_med), binwidth = 2000)


# scatterplots ----------
# create and save scatterplots of property vs income wealth, s per sq mi vs n schools,
# and poverty rate vs state + local pp rev

# plot prop value vs. income
ggplot(data = state_summary) +
  geom_point(mapping = aes(x = mpv_med, y = mhi_med)) +
  geom_smooth(mapping = aes(x = mpv_med, y = mhi_med))

# plot sd density vs number of schools
ggplot(data = state_summary) +
  geom_point(mapping = aes(x = student_per_sq_mile_med, y = n_schools_med))

# plot poverty vs. state + local pp rev
ggplot(data = state_summary) +
  geom_point(mapping = aes(x = st_pov_rate_med, y = state_loc_rev_pp_med)) + 
  geom_smooth(mapping = aes(x = st_pov_rate_med, y = state_loc_rev_pp_med))
