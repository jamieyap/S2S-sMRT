library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_heart_rate_indicator.RData"))
load(file = file.path(path_staged_data, "cstress_featurevec.RData"))
load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))

dat_included_masterlist <- dat_masterlist %>%
  filter(exclude_reason == "none") %>%
  select(participant_id, first_day_mrt, last_day_mrt)

all_included_participants <- dat_included_masterlist$participant_id

dat_cleaned_episodes_before_censoring <- dat_heart_rate_indicator
remove(dat_heart_rate_indicator)

# -----------------------------------------------------------------------------
# For episodes (any type) for which the time elapsed between peak and end
# exceeds 5 minutes, determine whether and how they should be censored
# -----------------------------------------------------------------------------
newlist_all <- list()
# Subsequent calculations within the loop depend upon correct time-ordering of rows in cstress_featurevec
cstress_featurevec <- cstress_featurevec %>% arrange(participant_id, cstress_featurevec_unixts)

for(i in 1:length(all_included_participants)){
  smalldat_episodes <- dat_cleaned_episodes_before_censoring %>% filter(participant_id == all_included_participants[i])
  smalldat_featurevec <- cstress_featurevec %>% filter(participant_id == all_included_participants[i])
  
  for(j in 1:nrow(smalldat_episodes)){
    current_start_time <- smalldat_episodes[j,"episode_start_hrts_local"]
    current_peak_time <- smalldat_episodes[j,"episode_peak_hrts_local"]
    current_end_time <- smalldat_episodes[j,"episode_end_hrts_local"]
    
    if(smalldat_episodes[j,"is_exceeds_5min"]==1 & smalldat_episodes[j,"is_exist_within_peak_and_end"]==1){
      featurevec_within_peak_and_end <- smalldat_featurevec %>% filter((cstress_featurevec_hrts_local >= current_peak_time) & (cstress_featurevec_hrts_local <= current_end_time))
      v <- c(current_peak_time, featurevec_within_peak_and_end[["cstress_featurevec_hrts_local"]], current_end_time)
      v_next <- c(featurevec_within_peak_and_end[["cstress_featurevec_hrts_local"]], current_end_time, NA)
      mins_between <- difftime(time1 = v_next, time2 = v, units = "mins")
      all_possible_idx <- which(mins_between > 5)
      
      # Un-comment for spot-checking
      # tmp <- data.frame(v = v, v_next = v_next, mins_between = mins_between)
      # print(tmp)
      
      if(length(all_possible_idx)>0){
        # Censor at the first time when there is 5 consecutive minutes of no heart rate data
        smalldat_episodes[j,"is_censored"] <- 1
        this_idx <- min(all_possible_idx)
        new_end_time <- v[this_idx]
        smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
        smalldat_episodes[j,"episode_newpeak_hrts_local"] <- current_peak_time
        smalldat_episodes[j,"episode_newend_hrts_local"] <- new_end_time
      }else{
        smalldat_episodes[j,"is_censored"] <- 0
        smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
        smalldat_episodes[j,"episode_newpeak_hrts_local"] <- current_peak_time
        smalldat_episodes[j,"episode_newend_hrts_local"] <- current_end_time
      }
    }else if(smalldat_episodes[j,"is_exceeds_5min"]==1 & smalldat_episodes[j,"is_exist_within_peak_and_end"]==0){
      # Censor at the peak (B)
      smalldat_episodes[j,"is_censored"] <- 1
      smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
      smalldat_episodes[j,"episode_newpeak_hrts_local"] <- current_peak_time
      smalldat_episodes[j,"episode_newend_hrts_local"] <- current_peak_time
    }else{
      # This covers the case when an episode does NOT exceed 5 minutes; no censoring occurs
      smalldat_episodes[j,"is_censored"] <- 0
      smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
      smalldat_episodes[j,"episode_newpeak_hrts_local"] <- current_peak_time
      smalldat_episodes[j,"episode_newend_hrts_local"] <- current_end_time
    }
  }
  
  newlist_all <- append(newlist_all, list(smalldat_episodes))
}

dat_cleaned_episodes_after_censoring <- do.call(rbind, newlist_all)

# -----------------------------------------------------------------------------
# Save intermediate output
# -----------------------------------------------------------------------------
save(dat_cleaned_episodes_after_censoring, file = file.path(path_staged_data, "dat_cleaned_episodes_after_censoring.RData"))

