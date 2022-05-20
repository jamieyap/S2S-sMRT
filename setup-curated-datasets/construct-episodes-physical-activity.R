library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))
load(file = file.path(path_staged_data, "parsed_dat_activity.RData"))
load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))

dat_included_masterlist <- dat_masterlist %>%
  filter(exclude_reason == "none") %>%
  select(participant_id, first_day_mrt, last_day_mrt)

all_included_participants <- dat_included_masterlist$participant_id

dat_cleaned_episodes <- parsed_dat_stress_episodes %>%
  select(participant_id, episode_id,
         orig_episode_classification,
         episode_start_hrts_local, episode_peak_hrts_local, episode_end_hrts_local)

# When event==1, participant is classified as physically active
# When event==0, participant is classified as NOT physically active
dat_physically_active <- parsed_dat_activity %>% filter(event==1)

# -----------------------------------------------------------------------------
# Remove observations not within 'first day' and 'last day' of the MRT
# -----------------------------------------------------------------------------
dat_cleaned_episodes <- left_join(x = dat_cleaned_episodes, y = dat_included_masterlist, by = "participant_id")

dat_cleaned_episodes <- dat_cleaned_episodes %>%
  filter((episode_peak_hrts_local >= first_day_mrt) & (episode_peak_hrts_local <= last_day_mrt))

# -----------------------------------------------------------------------------
# Save intermediate output
# -----------------------------------------------------------------------------

dat_cleaned_episodes %>%
  group_by(orig_episode_classification) %>%
  summarise(n())

# Rename data frame before saving
dat_valid_episodes <- dat_cleaned_episodes

save(dat_valid_episodes, file = file.path(path_staged_data, "dat_valid_episodes.RData"))

# -----------------------------------------------------------------------------
# Construct physical activity episodes
# -----------------------------------------------------------------------------
dat_cleaned_episodes$new_episode_classification <- dat_cleaned_episodes$orig_episode_classification
list_all <- list()

for(i in 1:length(all_included_participants)){
  smalldat_episodes <- dat_cleaned_episodes %>% filter(participant_id == all_included_participants[i])
  smalldat_activity <- dat_physically_active %>% filter(participant_id == all_included_participants[i])
  
  # Calculate the total number of physically active minutes between start and peak of an episode
  for(j in 1:nrow(smalldat_episodes)){
    current_start_time <- smalldat_episodes[j,"episode_start_hrts_local"]
    current_peak_time <- smalldat_episodes[j,"episode_peak_hrts_local"]
    current_class <- smalldat_episodes[j,"orig_episode_classification"]
    
    df_activity_within <- smalldat_activity %>% 
      filter((time_hrts_local>=current_start_time) & (time_hrts_local<=current_peak_time))
    
    # How many minutes between current_start_time and current_end_time 
    # were classified as physically active?
    count_minutes_within <- nrow(df_activity_within)
    # How many minutes are there between the start and peak of the episode?
    ab_len <- as.numeric(difftime(time1 = current_peak_time, time2 = current_start_time, units = "mins"))
    
    # If more then 50% of minutes between the start and peak of an unknown episode is physically active,
    # then regard the episode as physically active
    # else, still regard the episode as unknown
    if((count_minutes_within > ab_len/2) & (current_class == "unknown")){
      smalldat_episodes[j,"new_episode_classification"] <- "active"
    }
  }
  
  list_all <- append(list_all, list(smalldat_episodes))
}

dat_cleaned_episodes <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# Save intermediate output
# -----------------------------------------------------------------------------

dat_full_episodes <- dat_cleaned_episodes

save(dat_full_episodes, file = file.path(path_staged_data, "dat_full_episodes.RData"))

