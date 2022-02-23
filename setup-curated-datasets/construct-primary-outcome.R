library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))
load(file = file.path(path_staged_data, "parsed_dat_activity.RData"))
load(file = file.path(path_staged_data, "cstress_featurevec.RData"))
load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))
load(file = file.path(path_staged_data, "skeleton.RData"))

dat_included_masterlist <- dat_masterlist %>%
  filter(is.na(exclude_reason)) %>%
  select(participant_id, first_day_mrt, last_day_mrt)

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
# Construct physical activity episodes
# -----------------------------------------------------------------------------
all_included_participants <- dat_included_masterlist$participant_id
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
# For episodes (any type) greater than 5 minutes, determine whether
# they should be censored
# -----------------------------------------------------------------------------
newlist_all <- list()
dat_cleaned_episodes$is_exist_cstress_featurevec <- NA_real_

# Subsequent calculations within the loop depend upon correct time-ordering of rows in cstress_featurevec
cstress_featurevec <- cstress_featurevec %>% arrange(participant_id, cstress_featurevec_unixts)
# Calculate minutes elapsed between peak and end of an episode
dat_cleaned_episodes <- dat_cleaned_episodes %>% 
  mutate(mins_elapsed_peak_and_end = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                                         time2 = episode_peak_hrts_local, 
                                                         units = "mins"))) %>%
  mutate(is_greater_5min = if_else(mins_elapsed_peak_and_end > 5, 1, 0)) %>%
  mutate(is_greater_2min = if_else(mins_elapsed_peak_and_end > 2, 1, 0))
# Subsequent calculations within the loop depend upon unknown episodes having been removed from dat_cleaned_episodes
dat_cleaned_episodes <- dat_cleaned_episodes %>% filter(new_episode_classification!="unknown")

for(i in 1:length(all_included_participants)){
  smalldat_episodes <- dat_cleaned_episodes %>% filter(participant_id == all_included_participants[i])
  smalldat_featurevec <- cstress_featurevec %>% filter(participant_id == all_included_participants[i])
  
  # Identify the first time no cstress feature vector data occurred for 5 consecutive minutes after peak
  for(j in 1:nrow(smalldat_episodes)){
    current_peak_time <- smalldat_episodes[j,"episode_peak_hrts_local"]
    current_end_time <- smalldat_episodes[j,"episode_end_hrts_local"]
    featurevec_within <- smalldat_featurevec %>% filter((cstress_featurevec_hrts_local >= current_peak_time) & (cstress_featurevec_hrts_local <= current_end_time))
    num_mins_within <- nrow(featurevec_within)
    smalldat_episodes[j,"is_exist_cstress_featurevec"] <- if_else(num_mins_within==0, 0, 1)
  }
  
  newlist_all <- append(newlist_all, list(smalldat_episodes))
}

dat_cleaned_episodes_before_censoring <- do.call(rbind, newlist_all)

# Save intermediate output
save(dat_cleaned_episodes_before_censoring, file = file.path(path_staged_data, "dat_cleaned_episodes_before_censoring.RData"))

# Calculate summary statistics
table(dat_cleaned_episodes_before_censoring$is_greater_5min, dat_cleaned_episodes_before_censoring$is_exist_cstress_featurevec)



