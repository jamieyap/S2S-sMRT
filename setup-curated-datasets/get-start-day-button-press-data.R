library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

# -----------------------------------------------------------------------------
# We begin with log files for each participant, which have already been 
# pre-extracted from zip folders
# -----------------------------------------------------------------------------

all_file_names <- list.files(path = file.path(path_processed_data, "stress_cleaned_data"))

file_backup <- read.csv(file.path(path_processed_data, "stress_cleaned_data", "study_day_start_phone_backup.csv"))
ids_present_backup <- unique(file_backup$participant_id)
file_backup <- file_backup %>% select(timestamp, event, participant_id)  # Ensure column names are arranged in the same order before subsequent stacking
list_dat_all <- list(list(file_backup))

file_cloud <- read.csv(file.path(path_processed_data, "stress_cleaned_data", "study_day_start_phone_original.csv"))
ids_present_cloud <- unique(file_cloud$participant_id)
scan_these_ids <- setdiff(x = ids_present_cloud, y = ids_present_backup)
file_cloud <- file_cloud %>% filter(participant_id %in% scan_these_ids) %>% select(-offset)
file_cloud <- file_cloud %>% select(timestamp, event, participant_id)  # Ensure column names are arranged in the same order before subsequent stacking
list_dat_all <- append(list_dat_all, list(file_cloud))

file_alternative <- read.csv(file.path(path_processed_data, "stress_cleaned_data", "study_day_start_phone_alternative.csv"))
ids_present_alternative <- unique(file_alternative$participant_id)
scan_these_ids <- setdiff(x = ids_present_alternative, y = unique(c(ids_present_backup, ids_present_cloud)))
file_alternative <- file_alternative %>% filter(participant_id %in% scan_these_ids)
file_alternative <- file_alternative %>% select(timestamp, event, participant_id)  # Ensure column names are arranged in the same order before subsequent stacking
list_dat_all <- append(list_dat_all, list(file_alternative))

dat_all <- bind_rows(list_dat_all)
dat_all <- dat_all %>%
  mutate(time_unixts = timestamp/1000) %>%
  mutate(time_hrts_utc = as.POSIXct(x = time_unixts, tz = "UTC", origin = "1970-01-01")) %>%
  mutate(time_hrts_local = with_tz(time = time_hrts_utc, tzone = "America/Chicago")) %>%
  select(participant_id, time_hrts_utc, time_hrts_local, time_unixts)

# -----------------------------------------------------------------------------
# Investigate whether there are days where there are 2 or more recorded
# start of day button presses
# -----------------------------------------------------------------------------

dat_all <- dat_all %>% 
  mutate(date_utc = date(time_hrts_utc),
         date_local = date(time_hrts_local))

idx_dups <- duplicated(dat_all[,c("participant_id", "date_utc")])
idx_dups <- which(idx_dups)  # Inspect these elements

dat_all <- dat_all %>% filter(!duplicated(.[,c("participant_id", "date_utc")]))

# -----------------------------------------------------------------------------
# Prepare to save parsed data to an RData file in preparation for
# merging with other data sources
# -----------------------------------------------------------------------------

dat_all <- rename_with(dat_all, ~paste("day_start_", .x, sep=""), starts_with("time_"))
parsed_dat_day_start <- dat_all

save(parsed_dat_day_start, file = file.path(path_staged_data, "parsed_dat_day_start.RData"))

