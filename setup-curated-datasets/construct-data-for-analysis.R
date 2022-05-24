library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# -----------------------------------------------------------------------------
# Load relevant datasets
# -----------------------------------------------------------------------------
load(file.path(path_staged_data, "linked_dat.RData"))

# -----------------------------------------------------------------------------
# Create variables to be used in primary aim analysis
# -----------------------------------------------------------------------------
dat <- linked_dat %>%
  mutate(elapsed_secs = as.numeric(difftime(time1 = rand_time_hrts_local, 
                                            time2 = when_most_recent_classification, 
                                            units = "secs"))) %>%
  mutate(elapsed_mins = elapsed_secs/60) %>%
  mutate(is_any_classification = !is.na(most_recent_classification)) %>%
  mutate(is_within = if_else(elapsed_mins <= 5, 1, 0)) %>%
  mutate(case_indicator = case_when(
    isStress==1 & most_recent_classification=="yes" ~ 1,
    isStress==0 & most_recent_classification=="no" ~ 2,
    isStress==1 & most_recent_classification=="no" ~ 3,
    isStress==0 & most_recent_classification=="yes" ~ 4,
    TRUE ~ NA_real_
  )) %>% 
  select(-isStress)

k1 <- 0.05
k2 <- 0.95

dat <- dat %>%
  mutate(avail_raw = if_else(!is.na(trt_id), 1, 0)) %>%
  mutate(avail_aleph = case_when(
    is_any_classification == 0 ~ 0,
    most_recent_classification == "active" ~ 0,
    is_within == 0 ~ 0,
    probability == 0 ~ 0,
    probability == 1 ~ 0,
    TRUE  ~ 1
  )) %>%
  mutate(avail_omega = case_when(
    is_any_classification == 0 ~ 0,
    most_recent_classification == "active" ~ 0,
    is_within == 0 ~ 0,
    probability == 0 ~ 0,
    probability == 1 ~ 0,
    probability < k1 ~ 0,
    probability > k2 ~ 0,
    TRUE  ~ 1
  ))

dat <- dat %>%
  mutate(decision_now = isTriggered) %>%
  mutate(decision_now = replace(isTriggered, is.na(isTriggered), 0))

# -----------------------------------------------------------------------------
# Save only the variables you will need
# -----------------------------------------------------------------------------

dat_primary_aim <- dat %>%
  select(participant_id, mrt_day, mrt_minute, now_hrts_local, 
         classification,
         rand_time_hrts_local, probability, 
         most_recent_classification, when_most_recent_classification,
         isTriggered, decision_now,
         avail_raw, avail_aleph, avail_omega)

save(dat_primary_aim, file = file.path(path_staged_data, "dat_primary_aim.RData"))

