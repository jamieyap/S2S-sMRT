library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file.path(path_staged_data, "dat_masterlist.RData"))
load(file.path(path_staged_data, "cleaned_dat_rand.RData"))
load(file.path(path_staged_data, "cleaned_dat_day_start.RData"))
load(file.path(path_staged_data, "cleaned_dat_stress_episodes.RData"))
load(file.path(path_staged_data, "cleaned_dat_activity.RData"))

all_participant_ids <- dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  select(participant_id) %>% 
  .[["participant_id"]]

list_dat_all <- list()

for(idx_participant in 1:length(all_participant_ids)){
  
  curr_participant <- all_participant_ids[idx_participant]
  dat_subset_masterlist <- dat_masterlist %>% 
    filter(participant_id == curr_participant) %>%
    select(participant_id, 
           begin_study_date, 
           scheduled_visit_date, 
           actual_visit_date, 
           delayed_days, 
           end_study_date)
  
  dat <- data.frame(participant_id = rep(curr_participant, 7200))
  # Add more here later on
}






