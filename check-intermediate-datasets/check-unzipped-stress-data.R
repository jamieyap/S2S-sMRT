library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "unzipped_dat_stress_episodes.RData"))
load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))

# -----------------------------------------------------------------------------
# Only include data of participants not excluded due to C1, C2, C3, C4
# -----------------------------------------------------------------------------

included_ids <- dat_masterlist %>%
  filter(exclude_reason == "none") %>%
  .[["participant_id"]]

dat_masterlist_subset <- dat_masterlist %>% 
  filter(participant_id %in% included_ids) %>% 
  select(participant_id, first_day_mrt, last_day_mrt)

dat_stress_subset <- unzipped_dat_stress_episodes %>% 
  filter(participant_id %in% included_ids) %>%
  right_join(x = ., y = dat_masterlist_subset, by = "participant_id")

count_raw <- nrow(dat_stress_subset)

# -----------------------------------------------------------------------------
# Construct time variables
# -----------------------------------------------------------------------------

dat_stress_subset <- dat_stress_subset %>%
  # AC_secs -- seconds elapsed between beginning and end of an episode
  # AB_secs -- seconds elapsed between beginning and peak of an episode
  # BC_secs -- seconds elapsed between peak and end of an episode
  mutate(AC_secs = episode_end_unixts - episode_start_unixts,
         AB_secs = episode_peak_unixts - episode_start_unixts,
         BC_secs = episode_end_unixts - episode_peak_unixts) %>%
  mutate(AC_mins = AC_secs/60,
         AB_mins = AB_secs/60,
         BC_mins = BC_secs/60) %>%
  mutate(AC_hours = AC_mins/60,
         AB_hours = AB_mins/60,
         BC_hours = BC_mins/60) %>%
  arrange(participant_id, date_local, episode_start_hrts_local, AB_secs, AC_secs) 

# -----------------------------------------------------------------------------
# How many episodes were excluded because start time was January 1, 1970 UTC?
# -----------------------------------------------------------------------------

dat_stress_subset <- dat_stress_subset %>%
  mutate(is_stale = 1*(episode_start_hrts_utc < ymd_hms("2000-01-01 00:00:00", tz = "UTC")))

count_stale <- dat_stress_subset %>% filter(is_stale == 1) %>% nrow(.)
dat_stress_subset <- dat_stress_subset %>% filter(is_stale == 0)

# -----------------------------------------------------------------------------
# How many episodes were excluded because they were duplcates?
# -----------------------------------------------------------------------------

idx_start_only <- which(duplicated(dat_stress_subset[, c("participant_id", "episode_start_hrts_utc")]))
idx_start_and_peak <- which(duplicated(dat_stress_subset[, c("participant_id", "episode_start_hrts_utc", "episode_peak_hrts_utc")]))
when_unequal <- sum(idx_start_only!=idx_start_and_peak)
print(when_unequal)
these_duplicates <- duplicated(dat_stress_subset[, c("participant_id", "episode_start_hrts_utc", "episode_peak_hrts_utc")])
dat_stress_subset <- dat_stress_subset %>% mutate(is_duplicate = 1*these_duplicates)
count_duplicates <- dat_stress_subset %>% filter(is_duplicate == 1) %>% nrow(.)
dat_stress_subset <- dat_stress_subset %>% filter(is_duplicate == 0)

# -----------------------------------------------------------------------------
# How many episodes were excluded because the peak occurred before First Day 
# of the MRT or after Last Day of the MRT?
# -----------------------------------------------------------------------------

dat_stress_subset <- dat_stress_subset %>%
  mutate(is_after_last_day = 1*(episode_peak_hrts_local > last_day_mrt),
         is_before_first_day = 1*(episode_peak_hrts_local < first_day_mrt))

count_after_last_day <- dat_stress_subset %>% filter(is_after_last_day == 1) %>% nrow(.)
dat_stress_subset <- dat_stress_subset %>% filter(is_after_last_day == 0)

count_before_first_day <- dat_stress_subset %>% filter(is_before_first_day == 1) %>% nrow(.)
dat_stress_subset <- dat_stress_subset %>% filter(is_before_first_day == 0)

count_remaining_episodes <- nrow(dat_stress_subset)
print(count_remaining_episodes)

# -----------------------------------------------------------------------------
# Save counts to csv
# -----------------------------------------------------------------------------
dat_counts_unzipped_stress_data <- data.frame(count_raw = count_raw,
                                              count_stale = count_stale,
                                              count_duplicates = count_duplicates,
                                              count_after_last_day = count_after_last_day,
                                              count_before_first_day = count_before_first_day,
                                              count_remaining_episodes = count_remaining_episodes)

print(dat_counts_unzipped_stress_data)
write.csv(dat_counts_unzipped_stress_data, file.path("check-intermediate-datasets/collect-output", "dat_counts_unzipped_stress_data.csv"), row.names = FALSE, na = "")

