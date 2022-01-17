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
  filter(participant_id %in% all_participant_ids)

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
  duplicated(., fromLast = FALSE) %>%
  which(.)

idx_duplicates <- parsed_dat_day_start %>%
  select(participant_id, date_local) %>%
  duplicated(., fromLast = FALSE)

# One participant has more than one start-of-day timestamp on two different days
# Decision: Grab the earliest one. 
# The validity of this rule can also be checked
# by comparing 'remainingTimeInMinute' calculated by the software
# against remaining minutes out of the 720 minutes calculated 'by hand'

parsed_dat_day_start <- parsed_dat_day_start %>% filter(!idx_duplicates)

# -----------------------------------------------------------------------------
# Check validity of parsed day start times by comparing them against 
# 'remainingTimeInMinute' calculated by the software
# -----------------------------------------------------------------------------

parsed_dat_rand %>%
  select(participant_id, rand_time_hrts_local) %>%
  duplicated(.) %>%
  which(.)

parsed_dat_rand <- left_join(x = parsed_dat_rand,
                             y = parsed_dat_day_start,
                             by = c("participant_id", "date_local"))

parsed_dat_rand <- parsed_dat_rand %>%
  mutate(mins_elapsed = as.numeric(difftime(time1 = rand_time_hrts_local, 
                                            time2 = day_start_time_hrts_local, 
                                            units = "mins"))) %>%
  mutate(mins_remain = ceiling(720 - mins_elapsed))

# Any_unequal is an indicator for whether there exists conflicting information
# between two sources of data on when the participant initiated start-of-day

dat_summary_start_day <- parsed_dat_rand %>%
  group_by(participant_id, date_local) %>%
  summarise(any_unequal = 1 * (sum(remainingTimeInMinute!=mins_remain, na.rm=TRUE) > 0), .groups = "keep") %>%
  arrange(desc(any_unequal))

# Print out result of checks
print(dat_summary_start_day)

# Merge indicator with dataset containing the randomization assignments
parsed_dat_rand <- left_join(x = parsed_dat_rand, y = dat_summary_start_day, by = c("participant_id","date_local"))

# How many participant-decision points?
sum(parsed_dat_rand$any_unequal)

# -----------------------------------------------------------------------------
# Apply rule: if there exists conflicting information
# between two sources of data on when the participant initiated start-of-day,
# then use data from the data stream containing the randomization assignments
# -----------------------------------------------------------------------------

parsed_dat_rand <- parsed_dat_rand %>%
  mutate(new_day_start = as.POSIXct(NA)) %>%
  mutate(new_day_start = if_else(any_unequal==1, 
                                 rand_time_hrts_local - minutes(720 - remainingTimeInMinute), 
                                 new_day_start)) 

second(parsed_dat_rand[["new_day_start"]]) <- 0

dat_new_start_day <- parsed_dat_rand %>%
  filter(any_unequal == 1) %>%
  select(participant_id, date_local, any_unequal, new_day_start) %>%
  unique(.)

# -----------------------------------------------------------------------------
# Check whether coin flips exist on days for which day start times do not exist
# -----------------------------------------------------------------------------

dat_summary_coinflip <- parsed_dat_rand %>%
  group_by(participant_id, date_local) %>%
  summarise(any_rand_without_start_day = 1 * (sum(!is.na(rand_time_hrts_local) & is.na(day_start_time_hrts_local), na.rm=TRUE) > 0), 
            any_rand_with_start_day = 1 * (sum(!is.na(rand_time_hrts_local) & !is.na(day_start_time_hrts_local), na.rm=TRUE) > 0),
            .groups = "keep") %>%
  arrange(desc(any_rand_without_start_day))

# Print out result of checks
print(dat_summary_coinflip)

# -----------------------------------------------------------------------------
# More clean up
# -----------------------------------------------------------------------------

verified_dat_day_start <- left_join(x = parsed_dat_day_start, 
                                    y = dat_new_start_day,
                                    by = c("participant_id","date_local"))

verified_dat_day_start <- verified_dat_day_start %>%
  mutate(any_unequal = replace(any_unequal, is.na(any_unequal), 0))

verified_dat_day_start <- verified_dat_day_start %>%
  mutate(day_start_time_hrts_local = if_else(any_unequal == 1, new_day_start, day_start_time_hrts_local),
         day_start_time_hrts_utc = replace(day_start_time_hrts_utc, any_unequal == 1, NA),
         day_start_time_unixts = replace(day_start_time_unixts, any_unequal == 1, NA)) 

verified_dat_day_start <- verified_dat_day_start %>%
  mutate(day_start_time_hrts_utc = if_else(any_unequal == 1, with_tz(day_start_time_hrts_local, tz = "UTC"), day_start_time_hrts_utc)) %>%
  mutate(day_start_time_unixts = if_else(any_unequal == 1, as.numeric(day_start_time_hrts_utc), day_start_time_unixts))

verified_dat_day_start <- verified_dat_day_start %>%
  mutate(date_local = date(day_start_time_hrts_local),
         date_utc = date(day_start_time_hrts_utc))

verified_dat_day_start <- verified_dat_day_start %>% select(-new_day_start)

these_duplicates <- duplicated(verified_dat_day_start[,c("participant_id","date_local")])
idx_duplicates <- which(idx_duplicates)
print(idx_duplicates)

verified_dat_day_start <- verified_dat_day_start[!these_duplicates,]

# -----------------------------------------------------------------------------
# Save verified day start times
# -----------------------------------------------------------------------------

save(verified_dat_day_start, file = file.path(path_staged_data, "verified_dat_day_start.RData"))

