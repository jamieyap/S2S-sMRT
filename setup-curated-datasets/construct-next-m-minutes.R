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
  curr_list <- list(current_dat_rand = current_dat_rand_for_treatment_effect_estimation,
                    current_dat_classification = current_dat_minute_by_minute_classification)
  list_by_participant <- append(list_by_participant, list(curr_list))
}

# -----------------------------------------------------------------------------
# At the point of randomization t*, check the value of the 
# minute-by-minute trichotomous variable at a time point t*+m, 
# where m represents the number of minutes after t*
# -----------------------------------------------------------------------------

for(idx_this_participant in seq_len(tot_participants)){
  curr_list <- list_by_participant[[idx_this_participant]]
  current_dat_rand <- curr_list[["current_dat_rand"]]
  current_dat_classification <- curr_list[["current_dat_classification"]]
  
  num_rand <- nrow(current_dat_rand)
  look_ahead_mins <- 120
  list_collect <- list()
  
  for(i in seq_len(num_rand)){
    this_rand <- current_dat_rand[["rand_time_hrts_local"]][i]
    new_dat <- data.frame(current_timestamp = rep(this_rand, look_ahead_mins),
                          increment = seq_len(look_ahead_mins))
    new_dat <- new_dat %>% mutate(future_timestamp = current_timestamp + minutes(increment))
    
    all_minutes <- current_dat_classification[["now_hrts_local"]]
    all_classification <- current_dat_classification[["classification"]]
    
    for(j in seq_len(look_ahead_mins)){
      check_this_future_timestamp <- new_dat[["future_timestamp"]][j]
      this_idx <- max(which(check_this_future_timestamp >= all_minutes))
      matched_classification <- all_classification[this_idx]
      new_dat[["classification"]][j] <- matched_classification
    }
    
    mat <- matrix(new_dat[["classification"]], 
                  byrow=TRUE, 
                  nrow=1, 
                  dimnames = list(NULL, 
                                  paste("Y", seq_len(look_ahead_mins), 
                                        sep = "")))
    
    dat_future <- as.data.frame(mat)
    dat_future <- dat_future %>%
      mutate(rand_time_hrts_local = this_rand) %>%
      select(rand_time_hrts_local, everything())
    
    list_collect <- append(list_collect, list(dat_future))
  }
  
  dat_collect <- bind_rows(list_collect)
  updated_current_dat_rand <- left_join(x = current_dat_rand, 
                                        y = dat_collect,
                                        by = "rand_time_hrts_local")
  
  # Update list
  list_by_participant[[idx_this_participant]][["current_dat_rand"]] <- updated_current_dat_rand
}

list_dat_linked_future <- list()

for(idx_this_participant in seq_len(tot_participants)){
  curr_list <- list_by_participant[[idx_this_participant]]
  list_dat_linked_future <- append(list_dat_linked_future, list(curr_list[["current_dat_rand"]]))
}

dat_linked_future <- bind_rows(list_dat_linked_future)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

data_for_analysis <- dat_linked_future %>% select(-within_5min, -isStress)

save(data_for_analysis, file = file.path(path_staged_data, "data_for_analysis.RData"))

