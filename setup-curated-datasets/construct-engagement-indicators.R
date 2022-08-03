library(dplyr)
library(lubridate)
library(readxl)
source("paths.R")

load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))
load(file = file.path(path_staged_data, "skeleton.RData"))
load(file = file.path(path_staged_data, "dat_heart_rate_indicator.RData"))
load(file = file.path(path_staged_data, "cstress_featurevec.RData"))

# -----------------------------------------------------------------------------
# Read in file created by study staff and rename columns
# -----------------------------------------------------------------------------

dat_visit_tracking <- read_xlsx(file.path(path_raw_data_study_staff, "Sense2Stop_Participant_Dates.xlsx"),
                                col_types = c("numeric", "date", "date", "date", "date", "date", "numeric", "date", "text", "text"),
                                na = c(""))
# After reading in the excel file, "date" column types will be of variable type POSIXct
# Calling the function as_date() will coerce these columns to be of variable type Date
dat_visit_tracking <- dat_visit_tracking %>%
  select(`Participant ID`, `Completed Last Visit`) %>%
  rename(participant_id = `Participant ID`,
         date_completed_last_visit = `Completed Last Visit`) %>%
  mutate(date_completed_last_visit = as_date(date_completed_last_visit)) %>%
  mutate(is_last_visit_completed = !is.na(date_completed_last_visit)) %>%
  mutate(is_last_visit_completed = if_else(is_last_visit_completed==TRUE, 1, 0)) %>%
  select(-date_completed_last_visit)


dat_engagement <- left_join(x = dat_masterlist, y = dat_visit_tracking, by = "participant_id")
dat_engagement <- dat_engagement %>% 
  filter(exclude_reason == "none") %>%
  select(participant_id, is_last_visit_completed)

# -----------------------------------------------------------------------------
# Read in file created by study staff and rename columns
# -----------------------------------------------------------------------------

all_ids <- unique(dat_mrt_days$participant_id)
dat_mrt_days$any_hr_data <- NA_real_
list_all <- list()

for(i in 1:length(all_ids)){
  current_id <- all_ids[i]
  dat_days_current <- dat_mrt_days %>% filter(participant_id == current_id)
  dat_features_current <- cstress_featurevec %>% filter(participant_id == current_id)
  dates_database <- unique(lubridate::date(dat_features_current[["cstress_featurevec_hrts_local"]]))
  
  for(j in 1:nrow(dat_days_current)){
    this_date <- lubridate::date(dat_days_current[["date_local"]][j])
    count <- sum(this_date == dates_database)
    dat_days_current[j,"any_hr_data"] <- count
  }
  
  list_all <- append(list_all, list(dat_days_current))
}

dat_mrt_days <- do.call(rbind, list_all)
dat_mrt_days <- left_join(x = dat_mrt_days, y = dat_engagement, by = "participant_id")

# -----------------------------------------------------------------------------
# Visualize
# -----------------------------------------------------------------------------

plot(-1, -1, xlim = c(0,11), ylim = c(1,50), ylab = "Participant", xlab = "Day in MRT", frame = "false", xaxt = "n", yaxt = "n")
axis(1, at = 0:10)
axis(2, at = c(1, 10, 20, 30, 40, 49))

for(i in 1:length(all_ids)){
  abline(h = i, col = "grey")
  
  current_id <- all_ids[i]
  dat_days_current <- dat_mrt_days %>% filter(participant_id == current_id)
  with_hr <- dat_days_current$mrt_day[dat_days_current$any_hr_data==1]
  without_hr <- dat_days_current$mrt_day[dat_days_current$any_hr_data==0]
  if(length(with_hr)>0){
    points(with_hr, rep(i, length(with_hr)), col = "darkgoldenrod", pch = 16, cex = 2)
  }
  
  if(length(without_hr)>0){
    points(without_hr, rep(i, length(without_hr)), col = "black", pch = 16, cex = 2)
  }
  
  if(unique(dat_days_current[["is_last_visit_completed"]])==1){
    points(11, i, pch = 8, cex = 2, col = "darkgoldenrod", lwd = 3)
  }else{
    points(11, i, pch = 8, cex = 2, col = "black", lwd = 3)
  }
}

plot(-1, -1, xlim = c(0,11), ylim = c(1,50), ylab = "Participant", xlab = "Day in MRT", frame = "false", xaxt = "n", yaxt = "n")
axis(1, at = 0:10)
axis(2, at = c(1, 10, 20, 30, 40, 49))

for(i in 1:length(all_ids)){
  abline(h = i, col = "grey")
  
  current_id <- all_ids[i]
  dat_days_current <- dat_mrt_days %>% filter(participant_id == current_id)
  with_hr <- dat_days_current$mrt_day[dat_days_current$any_hr_data==1]
  without_hr <- dat_days_current$mrt_day[dat_days_current$any_hr_data==0]
  if(length(with_hr)>0){
    points(with_hr, rep(i, length(with_hr)), col = "darkgoldenrod", pch = 16, cex = 2)
  }
  
  if(unique(dat_days_current[["is_last_visit_completed"]])==1){
    points(11, i, pch = 8, cex = 2, col = "darkgoldenrod", lwd = 3)
  }else{
    points(11, i, pch = 8, cex = 2, col = "black", lwd = 3)
  }
}

# -----------------------------------------------------------------------------
# Save data
# -----------------------------------------------------------------------------

dat_engagement_indicators <- dat_mrt_days
save(dat_engagement_indicators, file = file.path(path_staged_data, "dat_engagement_indicators.RData"))

