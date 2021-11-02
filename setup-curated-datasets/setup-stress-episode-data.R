library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "skeleton.RData"))
load(file = file.path(path_staged_data, "parsed_dat_activity.RData"))
load(file = file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))

# -----------------------------------------------------------------------------
# Exclude data which are outside of the 720 minute time period;
# Exclude data on days when participants did not press start of day button
# -----------------------------------------------------------------------------

list_dat_episodes <- list()
list_dat_activity <- list()

all_participants <- unique(dat_mrt_days[["participant_id"]])
all_days <- unique(dat_mrt_days[["mrt_day"]])

for(current_participant in all_participants){
  for(current_day in all_days){
    
    # Calculate lower bound and upper bound
    # for each specific participant-day in the trial
    lower_bound <- dat_mrt_days %>%
      filter(participant_id == current_participant) %>%
      filter(mrt_day == current_day) %>%
      .[["day_start_time_hrts_local"]]
    
    upper_bound <- lower_bound + seconds(720*60)
    
    current_dat_episodes <- parsed_dat_stress_episodes %>%
      filter(participant_id == current_participant) %>%
      filter(Stress_Episode_Classification %in% c(0,2,3)) %>%
      # Condition 1: Episode is contained entirely within start and end of the 720 minute time period
      mutate(subset_condition_01 = (episode_start_hrts_local >= lower_bound & episode_start_hrts_local <= upper_bound & episode_end_hrts_local >= lower_bound & episode_end_hrts_local <= upper_bound),  
             # Condition 2: Episode began within the 720 minute time period but ended after the 720 minute time period 
             subset_condition_02 = (episode_start_hrts_local >= lower_bound & episode_start_hrts_local <= upper_bound & episode_end_hrts_local >= lower_bound & episode_end_hrts_local >= upper_bound),
             # Condition 3: Episode began prior to the 720 minute time period but ended within the 720 minute time period
             subset_condition_03 = (episode_start_hrts_local <= lower_bound & episode_start_hrts_local <= upper_bound & episode_end_hrts_local >= lower_bound & episode_end_hrts_local <= upper_bound)) %>%
      filter(subset_condition_01 | subset_condition_02 | subset_condition_03) %>%
      mutate(mrt_day = current_day) %>%
      select(participant_id, mrt_day, everything()) %>%
      select(-subset_condition_01, -subset_condition_02, -subset_condition_03)
    
    current_dat_activity <- parsed_dat_activity %>%
      filter(participant_id == current_participant) %>%
      filter((time_hrts_local >= lower_bound) & (time_hrts_local <= upper_bound)) %>%
      mutate(mrt_day = current_day) %>%
      select(participant_id, mrt_day, everything())
    
    list_dat_episodes <- append(list_dat_episodes, list(current_dat_episodes))
    list_dat_activity <- append(list_dat_activity, list(current_dat_activity))
  }
}

dat_episodes <- do.call(rbind, list_dat_episodes)
dat_activity <- do.call(rbind, list_dat_activity)

dat_episodes <- dat_episodes %>% arrange(participant_id, mrt_day, episode_start_unixts)
dat_activity <- dat_activity %>% arrange(participant_id, mrt_day, time_unixts)

# -----------------------------------------------------------------------------
# For episodes classified as 'UNKNOWN', re-label according to a rule
# -----------------------------------------------------------------------------

list_dat_episodes <- list()
list_dat_activity <- list()
dat_episodes$count_active_minutes <- NA_real_

all_participants <- unique(dat_mrt_days[["participant_id"]])

for(current_participant in all_participants){
  current_dat_activity <- dat_activity %>% filter(participant_id == current_participant)
  current_dat_episodes_unknown <- dat_episodes %>% filter(participant_id == current_participant) %>% filter(Stress_Episode_Classification == 3)
  current_dat_episodes_stressed <- dat_episodes %>% filter(participant_id == current_participant) %>% filter(Stress_Episode_Classification == 2)
  current_dat_episodes_not_stressed <- dat_episodes %>% filter(participant_id == current_participant) %>% filter(Stress_Episode_Classification == 0)
  
  if(nrow(current_dat_episodes_unknown) > 0){
    for(j in 1:nrow(current_dat_episodes_unknown)){
      this_start <- current_dat_episodes_unknown[j, "episode_start_hrts_local"]
      this_peak <- current_dat_episodes_unknown[j, "episode_peak_hrts_local"]
      these_idx <- (current_dat_activity[,"time_hrts_local"] >= this_start & current_dat_activity[,"time_hrts_local"] <= this_peak)
      current_dat_episodes_unknown[j, "count_active_minutes"] <- sum(these_idx)
    }
  }
  
  if(nrow(current_dat_episodes_stressed) > 0){
    for(j in 1:nrow(current_dat_episodes_stressed)){
      this_start <- current_dat_episodes_stressed[j, "episode_start_hrts_local"]
      this_peak <- current_dat_episodes_stressed[j, "episode_peak_hrts_local"]
      these_idx <- (current_dat_activity[,"time_hrts_local"] >= this_start & current_dat_activity[,"time_hrts_local"] <= this_peak)
      current_dat_episodes_stressed[j, "count_active_minutes"] <- sum(these_idx)
    }
  }
  
  if(nrow(current_dat_episodes_not_stressed) > 0){
    for(j in 1:nrow(current_dat_episodes_not_stressed)){
      this_start <- current_dat_episodes_not_stressed[j, "episode_start_hrts_local"]
      this_peak <- current_dat_episodes_not_stressed[j, "episode_peak_hrts_local"]
      these_idx <- (current_dat_activity[,"time_hrts_local"] >= this_start & current_dat_activity[,"time_hrts_local"] <= this_peak)
      current_dat_episodes_not_stressed[j, "count_active_minutes"] <- sum(these_idx)
    }
  }
  
  curr_list <- list()
  curr_list <- append(curr_list, list(current_dat_episodes_unknown))
  curr_list <- append(curr_list, list(current_dat_episodes_stressed))
  curr_list <- append(curr_list, list(current_dat_episodes_not_stressed))
  current_dat_episodes <- do.call(rbind, curr_list)
  
  list_dat_episodes <- append(list_dat_episodes, list(current_dat_episodes))
  list_dat_activity <- append(list_dat_activity, list(current_dat_activity))
}

dat_episodes <- do.call(rbind, list_dat_episodes)
dat_activity <- do.call(rbind, list_dat_activity)

dat_episodes <- dat_episodes %>% arrange(participant_id, mrt_day, episode_start_unixts)
dat_activity <- dat_activity %>% arrange(participant_id, mrt_day, time_unixts)

# -----------------------------------------------------------------------------
# Construct new stress classification variable which incorportes information
# from physical activity data stream
# -----------------------------------------------------------------------------

dat_episodes <- dat_episodes %>% 
  mutate(episode_classification = NA_character_) %>%
  mutate(episode_classification = case_when(
    Stress_Episode_Classification == 0 ~ "no",
    Stress_Episode_Classification == 2 ~ "yes",
    Stress_Episode_Classification == 3 & count_active_minutes >= AB_mins/2 ~ "active",
    TRUE ~ NA_character_
  ))

# -----------------------------------------------------------------------------
# Save datasets
# -----------------------------------------------------------------------------

save(dat_episodes, dat_activity, file = file.path(path_staged_data, "cleaned_dat_stress_episodes.RData"))

