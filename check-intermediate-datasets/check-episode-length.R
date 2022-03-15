library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_full_episodes.RData"))

# -----------------------------------------------------------------------------
# Number of minutes elapsed between B and C. 
# Statistics were calculated among episodes whose peak lies between 
# 'first day' and 'last day', inclusive
# -----------------------------------------------------------------------------

summarydat <- dat_full_episodes %>%
  mutate(BC_mins = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                       time2 = episode_peak_hrts_local, 
                                       units = "mins"))) %>%
  group_by(new_episode_classification) %>%
  summarise(q0 = quantile(BC_mins, probs = 0),
            q10 = quantile(BC_mins, probs = .10),
            q25 = quantile(BC_mins, probs = .25),
            q50 = quantile(BC_mins, probs = .50),
            q75 = quantile(BC_mins, probs = .75),
            q90 = quantile(BC_mins, probs = .90),
            q100 = quantile(BC_mins, probs = 1)) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    new_episode_classification == "unknown" ~ 4,
    TRUE ~ NA_real_)) %>%
  arrange(my_order) %>%
  select(-my_order)

summarydat[["q0"]] <- format(summarydat[["q0"]], nsmall=2, digits=2)
summarydat[["q10"]] <- format(summarydat[["q10"]], nsmall=2, digits=2)
summarydat[["q25"]] <- format(summarydat[["q25"]], nsmall=2, digits=2)
summarydat[["q50"]] <- format(summarydat[["q50"]], nsmall=2, digits=2)
summarydat[["q75"]] <- format(summarydat[["q75"]], nsmall=2, digits=2)
summarydat[["q90"]] <- format(summarydat[["q90"]], nsmall=2, digits=2)
summarydat[["q100"]] <- format(summarydat[["q100"]], nsmall=2, digits=2)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "beforecensor_duration.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Number of episodes for which the number of minutes elapsed between B and C is 
# greater than 5 minutes vs. at most 5 minutes
# -----------------------------------------------------------------------------

summarydat <- dat_full_episodes %>%
  mutate(BC_mins = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                       time2 = episode_peak_hrts_local, 
                                       units = "mins"))) %>%
  group_by(new_episode_classification) %>%
  summarise(cnt5 = sum(BC_mins>5),
            cntnot5 = sum(BC_mins<=5)) %>%
  mutate(tot = cnt5 + cntnot5) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    new_episode_classification == "unknown" ~ 4,
    TRUE ~ NA_real_)) %>%
  arrange(my_order) %>%
  select(-my_order) %>%
  add_row(new_episode_classification = "tot",
          cnt5 = sum(.[["cnt5"]]),
          cntnot5 = sum(.[["cntnot5"]]),
          tot = sum(.[["tot"]]))

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "beforecensor_count.csv"), row.names = FALSE)

