library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_matched_most_recent_classification.RData"))

# -----------------------------------------------------------------------------
# Micro-randomizations which will be used for treatment effect estimation
# must meet the all the following criteria:
#
#   * micro-randomizations for which the time between micro-randomization and 
#     most recent classification does not exceed 5 minutes
#   * micro-randomizations which occurred in episodes when an individual was 
#     classified as probably stressed or probably not stressed
#   * no mismatch in the value of the stratification variable among two data
#     sources (i.e., value of the stratification variable recorded in isStress 
#     vs. value of the stratification variable recorded in
#     most_recent_classification)
# -----------------------------------------------------------------------------
dat_rand_for_treatment_effect_estimation <- dat_matched_most_recent_classification %>%
  mutate(elapsed_secs = as.numeric(difftime(time1 = rand_time_hrts_local, 
                                            time2 = when_most_recent_classification, 
                                            units = "secs"))) %>%
  mutate(within_5min = if_else(elapsed_secs <= 5*60, 1, 0)) %>%
  filter(!is.na(most_recent_classification)) %>%
  filter(within_5min==1) %>%
  filter((isStress==0 & most_recent_classification=="no") | (isStress==1 & most_recent_classification=="yes"))

# -----------------------------------------------------------------------------
# Sanity check: After performing the steps above, does the expected number of
# rows remain in the dataset?
# -----------------------------------------------------------------------------
dat_summary <- dat_rand_for_treatment_effect_estimation %>%
  group_by(isStress, most_recent_classification, .groups = "keep") %>%
  summarise(tot_rand = n())

print(dat_summary)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------
save(dat_rand_for_treatment_effect_estimation, file = file.path(path_staged_data, "dat_rand_for_treatment_effect_estimation.RData"))

