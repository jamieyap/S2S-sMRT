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
tot_participants <- length(all_participant_ids)

# Read in censored episodes
# Note that 'unknown' episodes have been removed at this point
# so dat_cleaned_episodes_after_censoring should only contain
# stressed, not stressed, and physically active episodes
load(file.path(path_staged_data, "dat_cleaned_episodes_after_more_censoring.RData"))
# Grab only the columns you need
dat_episodes <- dat_cleaned_episodes_after_more_censoring %>%
  select(participant_id, episode_id, 
         new_episode_classification, 
         episode_newstart_hrts_local, episode_newend_hrts_local)

# Create a list for each participant
# This is the first step to being able to eventually parallelize
# data processing tasks using parLapply
list_by_participant <- list()
for(i in seq_len(tot_participants)){
  current_participant <- all_participant_ids[i]
  current_dat_mrt_minutes <- dat_mrt_minutes %>% filter(participant_id == current_participant)
  current_dat_episodes <- dat_episodes %>% filter(participant_id == current_participant)
  curr_list <- list(current_dat_mrt_minutes = current_dat_mrt_minutes,
                    current_dat_episodes = current_dat_episodes)
  list_by_participant <- append(list_by_participant, list(curr_list))
}

# For each minute, determine whether it lies within a probably stressed,
# probably not stressed, or physically active episode 
ncore <- detectCores()
cl <- makeCluster(ncore - 1)

list_linked <- parLapply(cl = cl,
                         X = list_by_participant,
                         fun = function(curr_list){
                           dat_all_mins <- curr_list[["current_dat_mrt_minutes"]]
                           dat_all_episodes <- curr_list[["current_dat_episodes"]]
                           dat_all_mins[["classification"]] <- NA_character_
                           tot_mins <- nrow(dat_all_mins)
                           start_timestamps <- dat_all_episodes[["episode_newstart_hrts_local"]]
                           end_timestamps <- dat_all_episodes[["episode_newend_hrts_local"]]
                           
                           for(idx in seq_len(tot_mins)){
                             now <- dat_all_mins[["now_hrts_local"]][idx]
                             this_index_within <- which((now >= start_timestamps) & (now < end_timestamps))
                             
                             if(length(this_index_within) == 0){
                               classification <- NA_character_
                             }else{
                               classification <- dat_all_episodes[["new_episode_classification"]][this_index_within]
                             }
                             
                             dat_all_mins[["classification"]][idx] <- classification
                           }
                           
                           # We have exited the for loop
                           return(dat_all_mins)
                         })

stopCluster(cl)

# Convert list to data frame
dat_minute_by_minute_classification <- bind_rows(list_linked)

# Save output
save(dat_minute_by_minute_classification, file = file.path(path_staged_data, "dat_minute_by_minute_classification.RData"))

