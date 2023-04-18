# exploring_the_tidyverse.R
# 2023-06-13

# load ----------

# set option to show non-scientific notation
options(scipen = 999)

library(tidyverse)

# load raw nces ccd finance data from fy19
# documentation available here: https://nces.ed.gov/ccd/pdf/2021306_FY19F33_Documentation.pdf
dist_finance_fy19_raw <- read_tsv("data/sdf19_1a.txt")

# clean with iterative tidyverse functions------------

# examine df
glimpse(dist_finance_fy19_raw)

# too many columns in raw df; select only the ones we want to see
dist_finance_fy19 <- select(dist_finance_fy19_raw,
                            LEAID, NAME, STABBR, GSLO, GSHI, MEMBERSCH,
                            TOTALREV, TFEDREV, TSTREV, TLOCREV)

# examine current df
glimpse(dist_finance_fy19)

# change column names
dist_finance_fy19 <- rename(dist_finance_fy19,
                            lea_id = LEAID,
                            district = NAME,
                            state = STABBR,
                            grade_low = GSLO,
                            grade_hi = GSHI,
                            enroll = MEMBERSCH, 
                            rev_total = TOTALREV,
                            rev_fed = TFEDREV,
                            rev_state = TSTREV,
                            rev_local = TLOCREV)

# examine current df
glimpse(dist_finance_fy19)

# remove districts with negative enrollment
dist_finance_fy19 <- filter(dist_finance_fy19, 
                            enroll > 0)

# examine current df
glimpse(dist_finance_fy19)

# create state and local total and per-pupil amounts 
dist_finance_fy19 <- mutate(dist_finance_fy19,
                            rev_state_local = rev_state + rev_local,
                            rev_pp_total = rev_total / enroll,
                            rev_pp_fed = rev_fed / enroll,
                            rev_pp_state = rev_state / enroll,
                            rev_pp_local = rev_local / enroll, 
                            rev_pp_state_local = rev_state_local / enroll)

# examine current df
glimpse(dist_finance_fy19)

# remove clean data 
rm(dist_finance_fy19)

# clean using nested tidyverse functions ---------
# this is an example of what NOT to do!
# to understand what is happening, you have to read from the inside-out

# start w/ raw data, then select columns, then rename columns, then filter
# rows, then mutate to create new columns
dist_finance_fy19 <- mutate(filter(rename(select(dist_finance_fy19_raw,
                                                 LEAID, NAME, STABBR, GSLO, GSHI, MEMBERSCH,
                                                 TOTALREV, TFEDREV, TSTREV, TLOCREV), 
                                          lea_id = LEAID,
                                          district = NAME,
                                          state = STABBR,
                                          grade_low = GSLO,
                                          grade_hi = GSHI,
                                          enroll = MEMBERSCH, 
                                          rev_total = TOTALREV,
                                          rev_fed = TFEDREV,
                                          rev_state = TSTREV,
                                          rev_local = TLOCREV), enroll > 0), 
                            rev_state_local = rev_state + rev_local,
                            rev_pp_total = rev_total / enroll,
                            rev_pp_fed = rev_fed / enroll,
                            rev_pp_state = rev_state / enroll,
                            rev_pp_local = rev_local / enroll, 
                            rev_pp_state_local = rev_state_local / enroll)


# remove clean data 
rm(dist_finance_fy19)

# clean using chained tidyverse functions ----------
# use the pipe ` |> ` to write a more readable set of nested commands
# when reading, think of ` |> ` as "and then"

# create a clean dist finance df by starting with our raw df
dist_finance_fy19 <- dist_finance_fy19_raw |> # and then
  # select only the columns we want to see
  select(LEAID, NAME, STABBR, GSLO, GSHI, MEMBERSCH,
         TOTALREV, TFEDREV, TSTREV, TLOCREV) |> # and then
  # change column names
  rename(lea_id = LEAID,
         district = NAME,
         state = STABBR,
         grade_low = GSLO,
         grade_hi = GSHI,
         enroll = MEMBERSCH, 
         rev_total = TOTALREV,
         rev_fed = TFEDREV,
         rev_state = TSTREV,
         rev_local = TLOCREV) |> # and then
  # remove districts with negative enrollment
  filter(enroll > 0) |> # and then
  # create state and local total and per-pupil amounts
  mutate(rev_state_local = rev_state + rev_local,
         rev_pp_total = rev_total / enroll,
         rev_pp_fed = rev_fed / enroll,
         rev_pp_state = rev_state / enroll,
         rev_pp_local = rev_local / enroll, 
         rev_pp_state_local = rev_state_local / enroll)

# examine current df
glimpse(dist_finance_fy19)

# when you filter out data, be sure to also explore those observations, too
# create a df that only includes districts with negative enrollment
dist_finance_fy19_no_enroll <- dist_finance_fy19_raw |> # and then
  # select only the columns we want to see
  select(LEAID, NAME, STABBR, GSLO, GSHI, MEMBERSCH,
         TOTALREV, TFEDREV, TSTREV, TLOCREV) |> # and then
  # change column names
  rename(lea_id = LEAID,
         district = NAME,
         state = STABBR,
         grade_low = GSLO,
         grade_hi = GSHI,
         enroll = MEMBERSCH, 
         rev_total = TOTALREV,
         rev_fed = TFEDREV,
         rev_state = TSTREV,
         rev_local = TLOCREV) |> # and then
  # keep districts with negative/zero enrollment
  filter(enroll <= 0) 

# open no enroll df in viewer
View(dist_finance_fy19_no_enroll)

# explore data with base r functions ---------

# use summary() to get stats on nationwide district enrollment
summary(dist_finance_fy19$enroll)

# use summary() to get stats on state + local per pupil revenue
summary(dist_finance_fy19$rev_pp_state_local)


# explore data using ggplot --------------

# create a histogram of state + local per pupil revenue
ggplot(data = dist_finance_fy19) +
  geom_histogram(mapping = aes(x = rev_pp_state_local))

# we can use chained commands within a function argument!
# use this approach sparingly and only during exploratory analysis
# exclude outliers by only keeping districts w/ < $50k per pupil
ggplot(data = dist_finance_fy19 |> filter(rev_pp_state_local < 50000)) +
  geom_histogram(mapping = aes(x = rev_pp_state_local))


# create outlier df for districts with state + local pp revenue < 500
dist_finance_fy19_lo_revenue <- dist_finance_fy19 |> 
  filter(rev_pp_state_local < 500) |> 
  arrange(-rev_pp_state_local)

# examine the low revenue df
View(dist_finance_fy19_lo_revenue)

# create outlier df for districts with state + local pp revenue > $50k
dist_finance_fy19_hi_revenue <- dist_finance_fy19 |> 
  filter(rev_pp_state_local >= 50000) |> 
  arrange(-rev_pp_state_local)

# examine the low revenue df
View(dist_finance_fy19_hi_revenue)


# clean our data, again --------

# create df with lo and hi outliers removed
dist_finance_fy19_no_outliers <- dist_finance_fy19 |> 
  filter(rev_pp_state_local < 50000) |>
  filter(rev_pp_state_local > 500)

# examine new df
glimpse(dist_finance_fy19_no_outliers)

# plot our data, again ----------

# create histogram of state + local pp revenue, sans outliers
ggplot(data = dist_finance_fy19_no_outliers) +
  geom_histogram(mapping = aes(x = rev_pp_state_local))

# reproduce histogram with bars representing $1k increments
ggplot(data = dist_finance_fy19_no_outliers) +
  geom_histogram(mapping = aes(x = rev_pp_state_local), binwidth = 1000)


# plot multiple variables -------------

# plot local pp revenue vs state pp revenue
ggplot(data = dist_finance_fy19_no_outliers) +
  geom_point(mapping = aes(x = rev_pp_local, 
                           y = rev_pp_state))

# recreate plot with point size mapped to enrollment
ggplot(data = dist_finance_fy19_no_outliers) +
  geom_point(mapping = aes(x = rev_pp_local, 
                           y = rev_pp_state,
                           size = enroll))

# adjust point opacity so they are only 20% visible
ggplot(data = dist_finance_fy19_no_outliers) +
  geom_point(mapping = aes(x = rev_pp_local, 
                           y = rev_pp_state,
                           size = enroll), 
             alpha = .2)

# save plot 
ggsave("figures/state_vs_local.png", units = "in",
       height = 5, width = 8)

# creating a summary df ---------------

state_summary_fy19 <- dist_finance_fy19_no_outliers |> 
  # group district df by state
  group_by(state) |>
  # for each unique value in the state column, a summary row will be
  # created using the following calculations
  summarise(n_dist = n(),
            enroll_total = sum(enroll),
            enroll_med = median(enroll),
            rev_pp_fed_med = median(rev_pp_fed),
            rev_pp_state_med = median(rev_pp_state),
            rev_pp_local_med = median(rev_pp_local),
            rev_pp_state_local_med = median(rev_pp_state_local))
