library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# -----------------------------------------------------------------------------
# Load relevant datasets
# -----------------------------------------------------------------------------
load(file.path(path_staged_data, "dat_rand_for_treatment_effect_estimation.RData"))
load(file.path(path_staged_data, "dat_minute_by_minute_classification.RData"))

# -----------------------------------------------------------------------------
# Transform dat_rand_for_treatment_effect_estimation and 
# dat_minute_by_minute_classification into a list, 
# where each element of the list corresponds to a data frame 
# containing data from only one participant.
# -----------------------------------------------------------------------------
all_participant_ids <- unique(dat_rand_for_treatment_effect_estimation[["participant_id"]])
tot_participants <- length(all_participant_ids)
list_by_participant <- list()

for(i in seq_len(tot_participants)){
  current_participant <- all_participant_ids[i]
  current_dat_rand_for_treatment_effect_estimation <- dat_rand_for_treatment_effect_estimation %>% filter(participant_id == current_participant)
  current_dat_minute_by_minute_classification <- dat_minute_by_minute_classification %>% filter(participant_id == current_participant)
  
  num_rand <- nrow(current_dat_rand_for_treatment_effect_estimation)
  current_dat_rand_for_treatment_effect_estimation <- current_dat_rand_for_treatment_effect_estimation %>% mutate(trt_id = 1:num_rand)
  current_dat_minute_by_minute_classification <- current_dat_minute_by_minute_classification %>% mutate(trt_id = NA_real_)
  
  curr_list <- list(current_dat_rand = current_dat_rand_for_treatment_effect_estimation,
                    current_dat_classification = current_dat_minute_by_minute_classification)
  list_by_participant <- append(list_by_participant, list(curr_list))
}

# -----------------------------------------------------------------------------
# Link randomization assignment to minute-by-minute trichotomous variable
# -----------------------------------------------------------------------------

list_linked_dat <- list()

for(idx_this_participant in seq_len(tot_participants)){
  curr_list <- list_by_participant[[idx_this_participant]]
  current_dat_classification <- curr_list[["current_dat_classification"]]
  current_dat_rand <- curr_list[["current_dat_rand"]]
  
  tot_minutes <- nrow(current_dat_classification)
  look_ahead_mins <- 1
  current_dat_classification <- current_dat_classification %>% mutate(next_hrts_local = now_hrts_local + minutes(look_ahead_mins))
  
  for(i in seq_len(tot_minutes)){
    check_this_current_timestamp <- current_dat_classification[["now_hrts_local"]][i]
    check_this_future_timestamp <- current_dat_classification[["next_hrts_local"]][i]
    this_idx <- which((current_dat_rand[["rand_time_hrts_local"]] >= check_this_current_timestamp) & (current_dat_rand[["rand_time_hrts_local"]] < check_this_future_timestamp))
    
    if(length(this_idx) > 0){
      matched_trt_id <- current_dat_rand[["trt_id"]][this_idx]
      current_dat_classification[["trt_id"]][i] <- matched_trt_id
    }
  }
  
  current_dat_classification <- left_join(x = current_dat_classification, y = current_dat_rand, by = c("participant_id","trt_id"))
  current_dat_classification <- current_dat_classification %>% select(-next_hrts_local)
  
  list_linked_dat <- append(list_linked_dat, list(current_dat_classification))
}

linked_dat <- do.call(rbind, list_linked_dat)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

save(linked_dat, file = file.path(path_staged_data, "linked_dat.RData"))

