library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

# -----------------------------------------------------------------------------
# We begin with log files for each participant, which have already been 
# pre-extracted from zip folders
# -----------------------------------------------------------------------------

all_file_names <- list.files(path = file.path(path_processed_data, "stress_cleaned_data"))

file_backup <- read.csv(file.path(path_processed_data, "stress_cleaned_data", "classification_full_episode_backup.csv"))
ids_present_backup <- unique(file_backup$participant_id)
file_backup <- file_backup %>% 
  select(timestamp, StartTime, PeakTime, EndTime, 
         Stress_Episode_Classification, 
         participant_id) # Ensure column names are arranged in the same order before subsequent stacking
list_dat_all <- list(list(file_backup))

file_cloud <- read.csv(file.path(path_processed_data, "stress_cleaned_data", "classification_full_episode_original.csv"))
ids_present_cloud <- unique(file_cloud$participant_id)
scan_these_ids <- setdiff(x = ids_present_cloud, y = ids_present_backup)
file_cloud <- file_cloud %>% filter(participant_id %in% scan_these_ids) %>% select(-offset)
file_cloud <- file_cloud %>% 
  select(timestamp, StartTime, PeakTime, EndTime, 
         Stress_Episode_Classification, 
         participant_id) # Ensure column names are arranged in the same order before subsequent stacking
list_dat_all <- append(list_dat_all, list(file_cloud))

file_alternative <- read.csv(file.path(path_processed_data, "stress_cleaned_data", "classification_full_episode_alternative.csv"))
ids_present_alternative <- unique(file_alternative$participant_id)
scan_these_ids <- setdiff(x = ids_present_alternative, y = unique(c(ids_present_backup, ids_present_cloud)))
file_alternative <- file_alternative %>% filter(participant_id %in% scan_these_ids)
file_alternative <- file_alternative %>% 
  select(timestamp, StartTime, PeakTime, EndTime, 
         Stress_Episode_Classification, 
         participant_id) # Ensure column names are arranged in the same order before subsequent stacking
list_dat_all <- append(list_dat_all, list(file_alternative))

dat_all <- bind_rows(list_dat_all)

dat_all <- dat_all %>%
  mutate(time_unixts = timestamp/1000,
         episode_start_unixts = StartTime/1000,
         episode_peak_unixts = PeakTime/1000,
         episode_end_unixts = EndTime/1000) %>%
  mutate(time_hrts_utc = as.POSIXct(x = time_unixts, tz = "UTC", origin = "1970-01-01"),
         episode_start_hrts_utc = as.POSIXct(x = episode_start_unixts, tz = "UTC", origin = "1970-01-01"),
         episode_peak_hrts_utc = as.POSIXct(x = episode_peak_unixts, tz = "UTC", origin = "1970-01-01"),
         episode_end_hrts_utc = as.POSIXct(x = episode_end_unixts, tz = "UTC", origin = "1970-01-01")) %>%
  mutate(time_hrts_local = with_tz(time = time_hrts_utc, tzone = "America/Chicago"),
         episode_start_hrts_local = with_tz(time = episode_start_hrts_utc, tzone = "America/Chicago"),
         episode_peak_hrts_local = with_tz(time = episode_peak_hrts_utc, tzone = "America/Chicago"),
         episode_end_hrts_local = with_tz(time = episode_end_hrts_utc, tzone = "America/Chicago")) %>%
  select(participant_id, 
         time_hrts_utc, time_hrts_local, time_unixts,
         episode_start_hrts_utc, episode_start_hrts_local, episode_start_unixts,
         episode_peak_hrts_utc, episode_peak_hrts_local, episode_peak_unixts,
         episode_end_hrts_utc, episode_end_hrts_local, episode_end_unixts,
         Stress_Episode_Classification) %>%
  # These columns are identical to episode_start_XX columns, so, drop from the data frame
  select(-time_hrts_utc, -time_hrts_local, -time_unixts)

# Create two additional date variables in preparation for subsequent merging on
# participant_id and calendar date
dat_all <- dat_all %>%
  mutate(date_utc = date(episode_start_hrts_utc),
         date_local = date(episode_start_hrts_local)) %>%
  select(participant_id, date_utc, date_local, everything())

# -----------------------------------------------------------------------------
# Calculate time duration variables
# -----------------------------------------------------------------------------

dat_all <- dat_all %>%
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
         BC_hours = BC_mins/60)

# -----------------------------------------------------------------------------
# Prepare to save parsed data to an RData file in preparation for
# merging with other data sources
# -----------------------------------------------------------------------------

# Check whether duplicates exist
print(sum(duplicated(dat_all)))

dat_all <- dat_all %>% 
  mutate(ones = 1) %>%
  mutate(episode_id = cumsum(ones)) %>%
  select(-ones)

parsed_dat_stress_episodes <- dat_all

save(parsed_dat_stress_episodes, file = file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))
