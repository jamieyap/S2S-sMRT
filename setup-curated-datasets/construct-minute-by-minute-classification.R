library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# Note that skeleton only contains participants 
# who were not labelled C1, C2, C3, C4
load(file.path(path_staged_data, "skeleton.RData"))
# Obtain all participant ID's which were not labelled C1, C2, C3, C4
all_participant_ids <- unique(dat_mrt_days$participant_id)

# Read in censored episodes
# Note that 'unknown' episodes have been removed at this point
# so dat_cleaned_episodes_after_censoring should only contain
# stressed, not stressed, and physically active episodes
load(file.path(path_staged_data, "dat_cleaned_episodes_after_more_censoring.RData"))
# Grab only the columns you need
dat_episodes <- dat_cleaned_episodes_after_censoring %>%
  select(participant_id, episode_id, 
         new_episode_classification, 
         episode_newstart_hrts_local, episode_newend_hrts_local)

# Create a list for each participant
# This is the first step to being able to eventually parallelize
# data processing tasks using parLapply

list_by_participant <- list()
for(i in seq_len(10)){
  current_participant <- all_participant_ids[i]
  current_dat_mrt_minutes <- dat_mrt_minutes %>% filter(participant_id == current_participant)
  current_dat_episodes <- dat_episodes %>% filter(participant_id == current_participant)
  curr_list <- list(current_dat_mrt_minutes = current_dat_mrt_minutes,
                    current_dat_episodes = current_dat_episodes)
  list_by_participant <- append(list_by_participant, list(curr_list))
}

# For each minute, determine whether it lies within a probably stressed,
# probably not stressed, or physically active episode 
dat_linked <- lapply(list_by_participant, function(curr_list){
  dat_all_mins <- curr_list[["current_dat_mrt_minutes"]]
  dat_all_episodes <- curr_list[["current_dat_episodes"]]
  
  dat_all_mins[["next_hrts_local"]] <- c(tail(dat_all_mins[["now_hrts_local"]], -1), as_datetime(NA))
  dat_out <- dat_all_mins[1:(nrow(dat_all_mins)-1),]
  tot_mins <- nrow(dat_out)
  
  for(idx in seq_len(tot_mins)){
    LB <- dat_out[["now_hrts_local"]][idx]
    UB <- dat_out[["next_hrts_local"]][idx]
    
    # Add here later
  }
})


