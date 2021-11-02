library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file.path(path_staged_data, "dat_masterlist.RData"))
load(file.path(path_staged_data, "verified_dat_day_start.RData"))

# -----------------------------------------------------------------------------
# For each day of the micro-randomized trial (MRT), determine whether a
# start time exists and cross-match start time for a particular date with
# day in MRT
# -----------------------------------------------------------------------------

dat_mrt_start <- dat_masterlist %>% 
  filter(exclude_from_all==0) %>%
  select(participant_id, begin_study_date, scheduled_visit_date, delayed_days, end_study_date) %>%
  mutate(mrt_day = 0) %>%
  mutate(total_mrt_days = as.numeric(difftime(time1 = end_study_date, time2 = scheduled_visit_date, units = "days")))

dat_mrt_days <- data.frame(participant_id = rep(dat_mrt_start[["participant_id"]], each = 11),
                           mrt_day = rep(0:10, times = length(dat_mrt_start[["participant_id"]])),
                           date_local = rep(dat_mrt_start[["scheduled_visit_date"]], each = 11),
                           delayed_days = rep(dat_mrt_start[["delayed_days"]], each = 11),
                           total_mrt_days = rep(dat_mrt_start[["total_mrt_days"]], each = 11))

dat_mrt_days <- dat_mrt_days %>%
  mutate(date_local = date_local + days(mrt_day)) 

dat_mrt_days <- left_join(x = dat_mrt_days, 
                          y = verified_dat_day_start, 
                          by = c("participant_id","date_local"))

# -----------------------------------------------------------------------------
# For each day within the MRT, determine which would have all 
# 12 hours * 60 mins = 720 minutes considered as ineligible for randomization
# -----------------------------------------------------------------------------

dat_mrt_days <- dat_mrt_days %>%
  mutate(is_day_avail = case_when(
    is.na(day_start_time_hrts_local) ~ 0,
    delayed_days==1 & mrt_day==0 ~ 0,
    total_mrt_days < mrt_day ~ 0,
    TRUE ~ 1
  ))

dat_mrt_days <- dat_mrt_days %>%
  select(-delayed_days, -total_mrt_days, -date_utc) %>%
  select(participant_id, mrt_day, 
         date_local, day_start_time_hrts_local, 
         everything())

# -----------------------------------------------------------------------------
# Construct minute-by-minute dataset
# -----------------------------------------------------------------------------

all_participant_ids <- unique(dat_mrt_days[["participant_id"]])
all_mrt_days <- unique(dat_mrt_days[["mrt_day"]])

list_dat <- list()

for(current_participant in all_participant_ids){
  for(current_day in all_mrt_days){
    
    # dat_current will be of class "data.frame" and will have one row
    dat_current <- dat_mrt_days %>%
      filter(participant_id == current_participant) %>%
      filter(mrt_day == current_day)
    
    dat_minute_level <- rep(list(dat_current), 721)
    dat_minute_level <- do.call(rbind, dat_minute_level)
    dat_minute_level <- dat_minute_level %>%
      mutate(mins_elapsed = 0:720) %>%
      mutate(now_hrts_local = as_datetime(NA),
             now_hrts_utc = as_datetime(NA),
             now_unixts = as_datetime(NA)) %>%
      mutate(now_hrts_utc = day_start_time_hrts_utc + seconds(mins_elapsed * 60)) %>%
      mutate(now_hrts_local = with_tz(now_hrts_utc, "America/Chicago"),
             now_unixts = as.numeric(as.POSIXct(x = now_hrts_utc, tz = "UTC", origin = "1970-01-01")))
    
    list_dat <- append(list_dat, list(dat_minute_level))
  }
}

dat_mrt_minutes <- do.call(rbind, list_dat)

# -----------------------------------------------------------------------------
# Save 'skeleton'
# -----------------------------------------------------------------------------

save(dat_mrt_days, dat_mrt_minutes, file = file.path(path_staged_data, "skeleton.RData"))

