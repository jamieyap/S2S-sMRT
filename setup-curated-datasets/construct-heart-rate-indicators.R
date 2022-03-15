library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_full_episodes.RData"))
load(file = file.path(path_staged_data, "cstress_featurevec.RData"))
load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))

dat_included_masterlist <- dat_masterlist %>%
  filter(exclude_reason == "none") %>%
  select(participant_id, first_day_mrt, last_day_mrt)

all_included_participants <- dat_included_masterlist$participant_id

# -----------------------------------------------------------------------------
# For episodes (any type) for which the time elapsed between peak and end
# exceeds 5 minutes, determine whether any heart rate data exists between 
# A and B, and between B and C
# -----------------------------------------------------------------------------
newlist_all <- list()

# Subsequent calculations within the loop depend upon correct time-ordering of rows in cstress_featurevec
cstress_featurevec <- cstress_featurevec %>% arrange(participant_id, cstress_featurevec_unixts)
# Calculate minutes elapsed between peak and end of an episode
dat_full_episodes <- dat_full_episodes %>% 
  mutate(mins_elapsed_peak_and_end = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                                         time2 = episode_peak_hrts_local, 
                                                         units = "mins"))) %>%
  mutate(is_exceeds_5min = if_else(mins_elapsed_peak_and_end > 5, 1, 0))
# Subsequent calculations within the loop depend upon unknown episodes having been removed from dat_full_episodes
dat_full_episodes <- dat_full_episodes %>% filter(new_episode_classification!="unknown")

for(i in 1:length(all_included_participants)){
  smalldat_episodes <- dat_full_episodes %>% filter(participant_id == all_included_participants[i])
  smalldat_featurevec <- cstress_featurevec %>% filter(participant_id == all_included_participants[i])
  
  for(j in 1:nrow(smalldat_episodes)){
    current_start_time <- smalldat_episodes[j,"episode_start_hrts_local"]
    current_peak_time <- smalldat_episodes[j,"episode_peak_hrts_local"]
    current_end_time <- smalldat_episodes[j,"episode_end_hrts_local"]
    
    # Check data between starting and peak of an episode
    featurevec_within_start_and_peak <- smalldat_featurevec %>% filter((cstress_featurevec_hrts_local >= current_start_time) & (cstress_featurevec_hrts_local <= current_peak_time))
    num_mins_within_start_and_peak <- nrow(featurevec_within_start_and_peak)
    smalldat_episodes[j,"is_exist_within_start_and_peak"] <- if_else(num_mins_within_start_and_peak==0, 0, 1)
    
    # Check data between peak and end of an episode
    featurevec_within_peak_and_end <- smalldat_featurevec %>% filter((cstress_featurevec_hrts_local >= current_peak_time) & (cstress_featurevec_hrts_local <= current_end_time))
    num_mins_within_peak_and_end <- nrow(featurevec_within_peak_and_end)
    smalldat_episodes[j,"is_exist_within_peak_and_end"] <- if_else(num_mins_within_peak_and_end==0, 0, 1)
  }
  
  newlist_all <- append(newlist_all, list(smalldat_episodes))
}

# Note that dat_heart_rate_indicator does not contain unknown episodes
# That is, it only contains stress, not stressed, and physically active episodes
dat_heart_rate_indicator <- do.call(rbind, newlist_all)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

save(dat_heart_rate_indicator, file = file.path(path_staged_data, "dat_heart_rate_indicator.RData"))

