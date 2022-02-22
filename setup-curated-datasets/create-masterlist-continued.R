library(dplyr)
library(lubridate)
source("paths.R")

load(file = file.path(path_staged_data, "dat_masterlist.RData"))
load(file = file.path(path_staged_data, "parsed_dat_rand.RData"))
load(file = file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))

# -----------------------------------------------------------------------------
# Create two variables to represent the 'first day' and 'last day' of the MRT
# -----------------------------------------------------------------------------
dat_masterlist <- dat_masterlist %>%
  mutate(first_day_mrt = actual_visit_date, 
         last_day_mrt = end_study_date + days(1)) %>%
  # Note: The next step will make first_day_mrt and last_day_mrt to be
  # at 12am America/Chicago time on the same calendar date
  mutate(first_day_mrt = force_tz(as_datetime(first_day_mrt), tz = "America/Chicago"),
         last_day_mrt = force_tz(as_datetime(last_day_mrt), tz = "America/Chicago")) %>%
  mutate(last_day_mrt = replace(last_day_mrt, is.na(first_day_mrt), as_datetime(NA)))

# -----------------------------------------------------------------------------
# Remove observations not within 'first day' and 'last day' of the MRT
# -----------------------------------------------------------------------------
parsed_dat_rand <- left_join(x = parsed_dat_rand,
                             y = dat_masterlist,
                             by = "participant_id")

parsed_dat_stress_episodes <- left_join(x = parsed_dat_stress_episodes,
                                        y = dat_masterlist,
                                        by = "participant_id")

parsed_dat_rand <- parsed_dat_rand %>%
  filter((rand_time_hrts_local >= first_day_mrt) & (rand_time_hrts_local <= last_day_mrt))

parsed_dat_stress_episodes <- parsed_dat_stress_episodes %>%
  filter((episode_peak_hrts_local >= first_day_mrt) & (episode_peak_hrts_local <= last_day_mrt))

# -----------------------------------------------------------------------------
# Identify participant ID's having at least 1 micro-randomization
# within 'first day' and 'last day' of the MRT
# -----------------------------------------------------------------------------
tabulate_dat_rand <- parsed_dat_rand %>%
  group_by(participant_id) %>%
  summarise(count_rand = n()) %>%
  arrange(desc(count_rand))

ids_with_rand <- tabulate_dat_rand[["participant_id"]]

# -----------------------------------------------------------------------------
# Identify participant ID's having at least 1 stress/not stress classification
# within 'first day' and 'last day' of the MRT
# -----------------------------------------------------------------------------
tabulate_dat_episodes <- parsed_dat_stress_episodes %>%
  group_by(participant_id) %>%
  summarise(count_episodes = n(),
            count_yes = sum(Stress_Episode_Classification == 2),
            count_no = sum(Stress_Episode_Classification == 0),
            count_unknown = sum(Stress_Episode_Classification == 3),
            count_not_unknown = sum(Stress_Episode_Classification != 3)) %>%
  arrange(desc(count_not_unknown))

ids_with_episodes <- tabulate_dat_episodes %>%
  filter(count_not_unknown > 0) %>%
  .[["participant_id"]]

# -----------------------------------------------------------------------------
# Update dat_masterlist
# -----------------------------------------------------------------------------
dat_masterlist <- dat_masterlist %>%
  mutate(any_rand = if_else(participant_id %in% ids_with_rand, 1, 0),
         any_episode = if_else(participant_id %in% ids_with_episodes, 1, 0)) %>%
  # Among participants not counted toward C1, C2, C3,
  # how many do we count toward C4?
  mutate(exclude_reason = replace(exclude_reason, is.na(exclude_reason) & (any_rand==0), "C4")) %>%
  arrange(exclude_reason)

# -----------------------------------------------------------------------------
# Which participants contribute towards C1, C2, C3, C4?
# -----------------------------------------------------------------------------
dat_masterlist %>% filter(exclude_reason == "C1") %>% .[["participant_id"]]
dat_masterlist %>% filter(exclude_reason == "C2") %>% .[["participant_id"]]
dat_masterlist %>% filter(exclude_reason == "C3") %>% .[["participant_id"]]
dat_masterlist %>% filter(exclude_reason == "C4") %>% .[["participant_id"]]

# How many participants contribute toward C1, C2, C3, C4?
table(dat_masterlist$exclude_reason)

# Among those participants who contribute toward C4
# how many did not have any stress/not stress episode classification
# between the first day and last day of the MRT?
table(dat_masterlist$exclude_reason=="C4", dat_masterlist$any_episode)

# Sanity check: if a participant did not contribute toward C1-C4, we expect
# them to have at least 1 stress/not stress episode classification
table(is.na(dat_masterlist$exclude_reason), dat_masterlist$any_episode)

# -----------------------------------------------------------------------------
# Calculate summary statistics on eligible minutes per participant
# Only calculate summary statistics among those participants who do not
# contribute toward C1, C2, C3, C4
# -----------------------------------------------------------------------------

ids_included <- dat_masterlist %>% 
  filter(is.na(exclude_reason)) %>% 
  .[["participant_id"]]

tabulate_dat_included_rand <- parsed_dat_rand %>%
  filter(participant_id %in% ids_included) %>%
  group_by(participant_id) %>%
  summarise(count_rand = n())

quantile(tabulate_dat_included_rand[["count_rand"]], c(0, .25, .50, .75, 1))

#0%  25%  50%  75% 100% 
#1   19   85  180  321 

# -----------------------------------------------------------------------------
# Prepare to save output
# -----------------------------------------------------------------------------

save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist_updated.RData"))

