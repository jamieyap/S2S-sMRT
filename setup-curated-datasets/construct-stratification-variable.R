library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# -----------------------------------------------------------------------------
# Remove observations not within 'first day' and 'last day' of the MRT
# -----------------------------------------------------------------------------
load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))
load(file = file.path(path_staged_data, "parsed_dat_rand.RData"))

parsed_dat_rand <- left_join(x = parsed_dat_rand, y = dat_masterlist, by = "participant_id")
dat_rand <- parsed_dat_rand %>%
  filter((rand_time_hrts_local >= first_day_mrt) & (rand_time_hrts_local <= last_day_mrt)) %>%
  filter(exclude_reason == "none") %>%
  select(participant_id, rand_time_hrts_local, probability, isTriggered)

# -----------------------------------------------------------------------------
# Transform dat_rand and dat_minute_by_minute_classification into a list, 
# where each element of the list corresponds to a data frame 
# containing data from only one participant.
# -----------------------------------------------------------------------------
load(file.path(path_staged_data, "dat_minute_by_minute_classification.RData"))

all_participant_ids <- unique(dat_rand$participant_id)
tot_participants <- length(all_participant_ids)
list_by_participant <- list()

for(i in seq_len(tot_participants)){
  current_participant <- all_participant_ids[i]
  current_dat_minute_by_minute_classification <- dat_minute_by_minute_classification %>% filter(participant_id == current_participant)
  current_dat_rand <- dat_rand %>% filter(participant_id == current_participant)
  curr_list <- list(current_dat_minute_by_minute_classification = current_dat_minute_by_minute_classification,
                    current_dat_rand = current_dat_rand)
  list_by_participant <- append(list_by_participant, list(curr_list))
}

# -----------------------------------------------------------------------------
# At the point of randomization t*, check the value of the 
# minute-by-minute trichotomous variable at a time point s, 
# where s represents the most recent classification prior to t*
# -----------------------------------------------------------------------------

list_dat_matched <- list()

for(i in seq_len(tot_participants)){
  current_dat_minute_by_minute_classification <- list_by_participant[[i]][["current_dat_minute_by_minute_classification"]]
  current_dat_rand <- list_by_participant[[i]][["current_dat_rand"]]
  tot_coinflips <- nrow(current_dat_rand)
  
  all_time <- current_dat_minute_by_minute_classification[["now_hrts_local"]]
  all_class <- current_dat_minute_by_minute_classification[["classification"]]
  
  for(j in seq_len(tot_coinflips)){
    this_timestamp <- current_dat_rand[["rand_time_hrts_local"]][j]
    collect_idx <- which((all_time <= this_timestamp) & (!is.na(all_class)))
    if(length(collect_idx) >= 1){
      this_idx <- max(collect_idx)
      most_recent_classification <- current_dat_minute_by_minute_classification[this_idx, "classification"]
      when_most_recent_classification <- current_dat_minute_by_minute_classification[this_idx, "now_hrts_local"]
      current_dat_rand[j, "most_recent_classification"] <- most_recent_classification
      current_dat_rand[j, "when_most_recent_classification"] <- when_most_recent_classification
    }else{
      current_dat_rand[j, "most_recent_classification"] <- NA_character_
      current_dat_rand[j, "when_most_recent_classification"] <- as_datetime(NA)
    }
  }
  
  list_dat_matched <- append(list_dat_matched, list(current_dat_rand))
}

dat_matched_most_recent_classification <- bind_rows(list_dat_matched)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------
save(dat_matched_most_recent_classification, file = file.path(path_staged_data, "dat_matched_most_recent_classification.RData"))

