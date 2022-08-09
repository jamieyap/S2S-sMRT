library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# -----------------------------------------------------------------------------
# Load relevant datasets
# -----------------------------------------------------------------------------
load(file = file.path(path_staged_data, "dat_primary_aim.RData"))
dat_demographics <- read.csv(file.path(path_raw_data_redcap, "Sense2Stop REDCap_Consented Participants_Demographics Surveys Assessments_RAW.csv"))

# -----------------------------------------------------------------------------
# Create time variables
# -----------------------------------------------------------------------------
dat_primary_aim <- dat_primary_aim %>%
  mutate(hour_of_day = lubridate::hour(now_hrts_local)) %>%
  mutate(part_of_day = case_when(
    (hour_of_day >= 5) & (hour_of_day <= 11) ~ "morning",
    (hour_of_day >= 12) & (hour_of_day <= 17) ~ "afternoon",
    (hour_of_day >= 18) & (hour_of_day <= 22) ~ "evening",
    TRUE ~ "night"
  ))

# -----------------------------------------------------------------------------
# Extract some demographics variables
# -----------------------------------------------------------------------------
dat_demographics <- dat_demographics %>% 
  select(record_id, sex) %>%
  rename(participant_id = record_id)

# -----------------------------------------------------------------------------
# Merge and save
# -----------------------------------------------------------------------------
dat_primary_aim <- left_join(x = dat_primary_aim, y = dat_demographics, by = "participant_id")

dat_primary_aim_with_predictors <- dat_primary_aim
save(dat_primary_aim_with_predictors, file = file.path(path_staged_data, "dat_primary_aim_with_predictors.RData"))

