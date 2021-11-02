library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file.path(path_staged_data, "parsed_dat_rand.RData"))
load(file.path(path_staged_data, "skeleton.RData"))

parsed_dat_rand <- parsed_dat_rand %>%
  select(participant_id, date_local,
         rand_time_hrts_local, rand_time_hrts_utc, rand_time_unixts,
         isPreLapse, isStress, 
         isTriggered, probability, emi_id)

all_participant_ids <- unique(dat_mrt_days[["participant_id"]])
all_days <- unique(dat_mrt_days[["mrt_day"]])

list_dat <- list()

for(current_participant in all_participant_ids){
  for(current_day in all_days){
    
    dat_current <- dat_mrt_minutes %>% 
      filter(participant_id == current_participant) %>%
      filter(mrt_day == current_day) %>%
      mutate(emi_id = NA_real_,
             lower_bound = as_datetime(NA),
             upper_bound = as_datetime(NA))
    
    is_day_avail <- dat_mrt_days %>% 
      filter(participant_id == current_participant) %>%
      filter(mrt_day == current_day) %>%
      select(is_day_avail) %>% 
      .[["is_day_avail"]]
    
    if(is_day_avail==1){
      dat_emi <- parsed_dat_rand %>% 
        filter(participant_id == current_participant)
      
      dat_current[["lower_bound"]] <- dat_current[["now_hrts_local"]]
      dat_current[["upper_bound"]] <- dat_current[["now_hrts_local"]] + minutes(1)
      
      for(current_row in 1:nrow(dat_current)){
        all_rand_times <- dat_emi[["rand_time_hrts_local"]]
        matched_idx <- which((all_rand_times >= dat_current[current_row,][["lower_bound"]]) & (all_rand_times < dat_current[current_row,][["upper_bound"]]))
        if(length(matched_idx)>0){
          dat_current[current_row,"emi_id"] <- dat_emi[matched_idx,][["emi_id"]]
        }
      }
    }
    
    list_dat <- append(list_dat, list(dat_current))
  }
}

dat_linked <- do.call(rbind, list_dat)

parsed_dat_rand <- parsed_dat_rand %>% select(-date_local)
dat_linked <- left_join(x = dat_linked, y = parsed_dat_rand, by = c("participant_id", "emi_id"))

dat_linked <- dat_linked %>%
  mutate(is_minute_avail = case_when(
    is_day_avail == 0 ~ 0,
    !is.na(emi_id) ~ 1,
    TRUE ~ 0
  ))

# -----------------------------------------------------------------------------
# Save linked data
# -----------------------------------------------------------------------------

save(dat_linked, file = file.path(path_staged_data, "dat_linked.RData"))

