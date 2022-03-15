library(dplyr)
library(lubridate)
source("paths.R")

load(file = file.path(path_staged_data, "dat_masterlist.RData"))
load(file = file.path(path_staged_data, "parsed_dat_rand.RData"))
load(file = file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))

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
# within 'first day' and 'last day' of the MRT (i.e., between the date times
# first_day_mrt and last_day_mrt, inclusive)
# -----------------------------------------------------------------------------
tabulate_dat_rand <- parsed_dat_rand %>%
  group_by(participant_id) %>%
  summarise(count_rand = n()) %>%
  arrange(desc(count_rand))

ids_with_rand <- tabulate_dat_rand[["participant_id"]]

# -----------------------------------------------------------------------------
# Update dat_masterlist
# -----------------------------------------------------------------------------
dat_masterlist <- dat_masterlist %>%
  mutate(any_rand = if_else(participant_id %in% ids_with_rand, 1, 0)) %>%
  # Among participants not counted toward C1, C2, C3,
  # how many do we count toward C4?
  mutate(exclude_reason = replace(exclude_reason, is.na(exclude_reason) & (any_rand==0), "C4")) %>%
  arrange(exclude_reason)

dat_masterlist <- dat_masterlist %>%
  mutate(exclude_reason = if_else(is.na(exclude_reason), "none", exclude_reason))

# -----------------------------------------------------------------------------
# Which participants contribute towards C1, C2, C3, C4?
# -----------------------------------------------------------------------------
dat_masterlist %>% filter(exclude_reason == "C1") %>% .[["participant_id"]]
dat_masterlist %>% filter(exclude_reason == "C2") %>% .[["participant_id"]]
dat_masterlist %>% filter(exclude_reason == "C3") %>% .[["participant_id"]]
dat_masterlist %>% filter(exclude_reason == "C4") %>% .[["participant_id"]]

# How many participants contribute toward C1, C2, C3, C4?
table(dat_masterlist$exclude_reason)

# -----------------------------------------------------------------------------
# Identify participant ID's having at least 1 stress/not stress classification
# within 'first day' and 'last day' of the MRT;
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

dat_masterlist <- dat_masterlist %>%
  mutate(any_episode = if_else(participant_id %in% ids_with_episodes, 1, 0))

# -----------------------------------------------------------------------------
# Among those participants who contribute toward C4
# how many did not have any stress/not stress episode classification
# between the first day and last day of the MRT?
# -----------------------------------------------------------------------------
dat_masterlist %>%
  filter(exclude_reason=="C4") %>%
  summarise(count_with_episode = sum(any_episode==1),
            count_without_episode = sum(any_episode==0))

# -----------------------------------------------------------------------------
# Prepare to save output
# -----------------------------------------------------------------------------

dat_masterlist <- dat_masterlist %>% select(-any_rand, -any_episode)

save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist_updated.RData"))

