library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file.path(path_staged_data, "dat_masterlist.RData"))
load(file.path(path_staged_data, "parsed_dat_rand.RData"))
load(file.path(path_staged_data, "parsed_dat_day_start.RData"))

all_participant_ids <- dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  select(participant_id) %>% 
  .[["participant_id"]]

parsed_dat_day_start <- parsed_dat_day_start %>% 
  filter(participant_id %in% all_participant_ids) %>%
  select(participant_id, 
         date_local, 
         day_start_time_hrts_local)

parsed_dat_rand <- parsed_dat_rand %>% 
  filter(participant_id %in% all_participant_ids) %>%
  select(participant_id, 
         date_local, 
         rand_time_hrts_local, 
         remainingTimeInMinute)

# -----------------------------------------------------------------------------
# Check whether there are days when there are two or more day start timestamps
# -----------------------------------------------------------------------------

# Note that this only occurs on 2 participant-days
# In one of the participant-days for which duplicates occurred, no
# randomizations occurred. In one of the other participant-days for which
# duplicates occurred, randomizations did occur.
parsed_dat_day_start %>%
  select(participant_id, date_local) %>%
  duplicated(.) %>%
  which(.)

idx_duplicates <- parsed_dat_day_start %>%
  select(participant_id, date_local) %>%
  duplicated(.)

# If two or more day start timestamps exist on a given participant-day
# grab the earliest one; the validity of this rule can also be checked
# by comparing 'remainingTimeInMinute' calculated by the software
# against remaining minutes out of the 720 minutes calculated 'by hand'

cleaned_dat_day_start <- parsed_dat_day_start %>% filter(!idx_duplicates)

# -----------------------------------------------------------------------------
# Check validity of parsed day start times by comparing them against 
# 'remainingTimeInMinute' calculated by the software
# -----------------------------------------------------------------------------

parsed_dat_rand <- left_join(x = parsed_dat_rand,
                             y = cleaned_dat_day_start,
                             by = c("participant_id", "date_local"))

parsed_dat_rand <- parsed_dat_rand %>%
  mutate(mins_elapsed = as.numeric(difftime(time1 = rand_time_hrts_local, 
                                            time2 = day_start_time_hrts_local, 
                                            units = "mins"))) %>%
  mutate(mins_remain = ceiling(720 - mins_elapsed))

dat_summary_start_day <- parsed_dat_rand %>%
  group_by(participant_id, date_local) %>%
  summarise(any_unequal = 1 * (sum(remainingTimeInMinute!=mins_remain, na.rm=TRUE) > 0), .groups = "keep") %>%
  arrange(desc(any_unequal))

# -----------------------------------------------------------------------------
# Check whether coin flips exist on days for which day start times do not exist
# -----------------------------------------------------------------------------

dat_summary_coinflip <- parsed_dat_rand %>%
  group_by(participant_id, date_local) %>%
  summarise(any_rand_without_start_day = 1 * (sum(!is.na(rand_time_hrts_local) & is.na(day_start_time_hrts_local), na.rm=TRUE) > 0), 
            any_rand_with_start_day = 1 * (sum(!is.na(rand_time_hrts_local) & !is.na(day_start_time_hrts_local), na.rm=TRUE) > 0),
            .groups = "keep") %>%
  arrange(desc(any_rand_without_start_day))

# -----------------------------------------------------------------------------
# Save cleaned up day start times: Need to load original 
# parsed_dat_day_start.RData file again so that we keep the other columns
# in that file which were dropped prior to performing the checks above.
# We also 'clean up' the parsed randomization data.
# -----------------------------------------------------------------------------

load(file.path(path_staged_data, "dat_masterlist.RData"))
load(file.path(path_staged_data, "parsed_dat_rand.RData"))
load(file.path(path_staged_data, "parsed_dat_day_start.RData"))

all_participant_ids <- dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  select(participant_id) %>% 
  .[["participant_id"]]

cleaned_dat_day_start <- parsed_dat_day_start %>% 
  filter(participant_id %in% all_participant_ids) %>%
  filter(!idx_duplicates)

cleaned_dat_rand <- parsed_dat_rand %>% 
  filter(participant_id %in% all_participant_ids)

save(cleaned_dat_day_start, file = file.path(path_staged_data, "cleaned_dat_day_start.RData"))
save(cleaned_dat_rand, file = file.path(path_staged_data, "cleaned_dat_rand.RData"))

