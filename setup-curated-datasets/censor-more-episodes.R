library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_cleaned_episodes_after_censoring.RData"))

# -----------------------------------------------------------------------------
# If the length of time between the start (A) and end (C) of an episode is 
# greater than 17 minutes, censor at 17 minutes
# -----------------------------------------------------------------------------

here <- 17

dat_cleaned_episodes_after_more_censoring <- dat_cleaned_episodes_after_censoring %>%
  mutate(AC_mins = as.numeric(difftime(time1 = episode_newend_hrts_local,
                                       time2 = episode_newstart_hrts_local,
                                       units = "mins"))) %>%
  mutate(is_more_censored = if_else(AC_mins > here, 1, 0),
         max_end = episode_newstart_hrts_local + minutes(here)) %>%
  mutate(episode_newend_hrts_local = if_else(is_more_censored==1, 
                                             max_end, 
                                             episode_newend_hrts_local)) %>%
  select(-max_end, -AC_mins) %>%
  mutate(any_censored = if_else(is_censored + is_more_censored > 0, 1, 0))

save(dat_cleaned_episodes_after_more_censoring, 
     file = file.path(path_staged_data, "dat_cleaned_episodes_after_more_censoring.RData"))

