library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# -----------------------------------------------------------------------------
# Load relevant datasets
# -----------------------------------------------------------------------------
load(file.path(path_staged_data, "linked_dat.RData"))

data_for_analysis <- linked_dat

# -----------------------------------------------------------------------------
# Only select columns you will need
# -----------------------------------------------------------------------------
data_for_analysis <- data_for_analysis %>% select(-within_5min, -isStress)

# -----------------------------------------------------------------------------
# Create indicator for availability
# -----------------------------------------------------------------------------
data_for_analysis <- data_for_analysis %>%
  mutate(avail_raw = if_else(is.na(trt_id), 0, 1)) %>%
  mutate(avail_new = avail_raw) %>%
  mutate(avail_new = replace(avail_new, (probability == 0) | (probability == 1), 0))

# -----------------------------------------------------------------------------
# Create composite indicator combining randomization assignment and whether
# any micro-randomization occurred
# -----------------------------------------------------------------------------
data_for_analysis <- data_for_analysis %>%
  mutate(composite = case_when(
    is.na(isTriggered) ~ 0,
    !is.na(isTriggered) & (isTriggered==0) ~ 0,
    TRUE ~ 1
  ))

# -----------------------------------------------------------------------------
# Create missing data indicator
# -----------------------------------------------------------------------------
data_for_analysis <- data_for_analysis %>%
  mutate(is_classification_observed = if_else(is.na(classification), 0, 1))

# -----------------------------------------------------------------------------
# Rename and rearrange variables prior to saving
# -----------------------------------------------------------------------------
data_for_analysis <- data_for_analysis %>%
  rename(Yit = classification,
         Xit = most_recent_classification,
         pit = probability,
         Mit = is_classification_observed,
         Ait = composite,
         Iit = avail_new)

data_for_analysis <- data_for_analysis %>%
  select(participant_id,
         mrt_day, mrt_minute, date_local, now_hrts_local,
         Yit, Xit, Iit, Ait, pit, Mit,
         first_day_mrt, last_day_mrt,
         everything())

# -----------------------------------------------------------------------------
# Add documentation on variables in the data frame data_for_analysis
# -----------------------------------------------------------------------------
attributes(data_for_analysis)$data.dictionary <- list("participant_id" = "A unique identifier for each participant enrolled in the study", 
                                                      "mrt_day" = "Number of days elapsed since first day of mrt",
                                                      "mrt_minute" = "Minutes elapsed since 12AM of date_local",
                                                      "date_local" = "Calendar date when mrt_day took place; this variable is displayed in YYY-MM-DD format",
                                                      "now_hrts_local" = "Specific time of day within date_local; this variable is displayed in YYY-MM-DD HH:MM:SS format",
                                                      "Yit" = "A categorical variable to indicate whether an individual is probably stressed (=yes), probably not stressed (=no) or physically active (=active) at the timestamp given by now_hrts_local",
                                                      "Xit" = "A categorical variable to indicate whether micro-randomization occurred when the individual was probably stressed (=yes) or probably not stressed (=no) at the timestamp given by now_hrts_local",
                                                      "Iit" = "Availability at the timestamp given by now_hrts_local (this is the modified definition of availability)",
                                                      "Ait" = "A composite indicator combining randomization assignment and whether any micro-randomization occurred at the timestamp given by now_hrts_local",
                                                      "pit" = "The probability that an individual will be micro-randomized to prompt at the timestamp given by now_hrts_local. That is, at the timestamp given by now_hrts_local, the probability that isTriggered=1. Conversely, 1-probability is the probability that an individual will be micro-randomized to no prompt; that is, the probability that isTriggered=0",
                                                      "Mit" = "Whether any classification was observed at the timestamp given by now_hrts_local",
                                                      "first_day_mrt" = "Calendar date when first day of MRT took place; this variable is displayed in YYY-MM-DD format",
                                                      "last_day_mrt" = "Calendar date when last day of MRT took place; this variable is displayed in YYY-MM-DD format",
                                                      "trt_id" = "A unique identifier within each participant of the randomization assignments",
                                                      "rand_time_hrts_local" = "Timestamp corresponding to when micro-randomization occurred; note that rand_time_hrts_local should always be within 1 mnute of now_hrts_local",
                                                      "isTriggered" = "A categorical variable to indicate whether the individual was micro-randomized to prompt (=1) or no prompt (=0)",
                                                      "when_most_recent_classification" = "Timestamp corresponding to the most recent classification (given by the variable most_recent_classification) available prior to micro-randomization",
                                                      "elapsed_secs" = "Difference between rand_time_hrts_local and when_most_recent_classification in seconds",
                                                      "avail_raw" = "Availability for intervention, as determined by software")

# This operation transforms data.dictionary into a column vector
attributes(data_for_analysis)$data.dictionary <- do.call(rbind, attributes(data_for_analysis)$data.dictionary)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------
save(data_for_analysis, file = file.path(path_staged_data, "data_for_analysis.RData"))



