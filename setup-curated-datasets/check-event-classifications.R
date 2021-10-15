library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file.path(path_staged_data, "dat_masterlist.RData"))
load(file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))
load(file.path(path_staged_data, "parsed_dat_activity.RData"))

all_participant_ids <- dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  select(participant_id) %>% 
  .[["participant_id"]]

parsed_dat_stress_episodes <- parsed_dat_stress_episodes %>% 
  filter(participant_id %in% all_participant_ids)

parsed_dat_activity <- parsed_dat_activity %>% 
  filter(participant_id %in% all_participant_ids)

# -----------------------------------------------------------------------------
# Check number of rows for each possible classification,
# and percentiles of number of minutes per episode by classification
# -----------------------------------------------------------------------------

parsed_dat_stress_episodes %>%
  group_by(Stress_Episode_Classification) %>%
  summarise(count = n(),
            mins_q10 = quantile(AC_mins, .10),
            mins_q50 = quantile(AC_mins, .50),
            mins_q90 = quantile(AC_mins, .90))

# Inspect the 21 episodes classified as a '4':
parsed_dat_stress_episodes %>%
  filter(Stress_Episode_Classification==4) %>%
  View(.)

# See whether any duplicates exist;
# zero duplicates exist
sum(duplicated(parsed_dat_stress_episodes))

parsed_dat_activity %>%
  group_by(event) %>%
  summarise(count = n())

# See whether any duplicates exist
# Only 2 rows are duplicates
sum(duplicated(parsed_dat_activity))

# -----------------------------------------------------------------------------
# Perform more data preparation steps
# -----------------------------------------------------------------------------

parsed_dat_stress_episodes <- parsed_dat_stress_episodes %>%
  filter(Stress_Episode_Classification!=4)

idx_duplicated <- duplicated(parsed_dat_activity)
parsed_dat_activity <- parsed_dat_activity %>% filter(!idx_duplicated)

# -----------------------------------------------------------------------------
# Save cleaned data
# -----------------------------------------------------------------------------

cleaned_dat_stress_episodes <- parsed_dat_stress_episodes
cleaned_dat_activity <- parsed_dat_activity

save(cleaned_dat_stress_episodes, file = file.path(path_staged_data, "cleaned_dat_stress_episodes.RData"))
save(cleaned_dat_activity, file = file.path(path_staged_data, "cleaned_dat_activity.RData"))

