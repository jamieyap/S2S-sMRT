library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file.path(path_staged_data, "dat_masterlist_updated.RData"))

dat_masterlist_included <- dat_masterlist %>% filter(exclude_reason == "none")

# -----------------------------------------------------------------------------
# Create a data frame in long-format where each row represents one
# participant-day
# -----------------------------------------------------------------------------

list_all <- list()

for(i in 1:nrow(dat_masterlist_included)){
  current_participant_id <- dat_masterlist_included[["participant_id"]][i]
  current_first_day_mrt <- dat_masterlist_included[["first_day_mrt"]][i]
  current_last_day_mrt <- dat_masterlist_included[["last_day_mrt"]][i]
  
  # Number of days elapsed between 12AM on current_first_day_mrt
  # and 12AM on current_last_day_mrt
  current_total_mrt_days = as.numeric(difftime(time1 = current_last_day_mrt, 
                                               time2 = current_first_day_mrt, 
                                               units = "days"))
  
  dat <- data.frame(participant_id = rep(current_participant_id, each = current_total_mrt_days),
                    first_day_mrt = rep(current_first_day_mrt, each = current_total_mrt_days),
                    last_day_mrt = rep(current_last_day_mrt, each = current_total_mrt_days),
                    mrt_day = 0:(current_total_mrt_days-1))
  
  dat <- dat %>% mutate(date_local = first_day_mrt + days(mrt_day))
  list_all <- append(list_all, list(dat))
}

dat_mrt_days <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# Create a data frame in long-format where each row represents one
# participant-day-minute
# -----------------------------------------------------------------------------

list_all <- list()

for(i in 1:nrow(dat_mrt_days)){
  current_participant_id <- dat_mrt_days[["participant_id"]][i]
  current_first_day_mrt <- dat_mrt_days[["first_day_mrt"]][i]
  current_last_day_mrt <- dat_mrt_days[["last_day_mrt"]][i]
  current_mrt_day <- dat_mrt_days[["mrt_day"]][i]
  current_date_local <- dat_mrt_days[["date_local"]][i]
  
  dat <- data.frame(participant_id = rep(current_participant_id, each = 24*60),
                    first_day_mrt = rep(current_first_day_mrt, each = 24*60),
                    last_day_mrt = rep(current_last_day_mrt, each = 24*60),
                    mrt_day = rep(current_mrt_day, each = 24*60),
                    date_local = rep(current_date_local, each = 24*60),
                    mrt_minute = 0:(24*60-1))
  
  dat <- dat %>% mutate(now_hrts_local = date_local + minutes(mrt_minute))
  list_all <- append(list_all, list(dat))
}

dat_mrt_minutes <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

save(dat_mrt_days, dat_mrt_minutes, file = file.path(path_staged_data, "skeleton.RData"))

