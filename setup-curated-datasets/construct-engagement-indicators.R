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


dat_visit_attendance <- left_join(x = dat_masterlist, y = dat_visit_tracking, by = "participant_id")
dat_visit_attendance <- dat_visit_attendance %>% 
  filter(exclude_reason == "none") %>%
  select(participant_id, is_last_visit_completed)

# -----------------------------------------------------------------------------
# Read in file created by study staff and rename columns
# -----------------------------------------------------------------------------

all_ids <- unique(dat_mrt_days$participant_id)
dat_mrt_days$any_hr_data_today <- NA_real_
list_all <- list()

for(i in 1:length(all_ids)){
  current_id <- all_ids[i]
  dat_days_current <- dat_mrt_days %>% filter(participant_id == current_id)
  
  current_first_day_mrt <- dat_days_current[["first_day_mrt"]][1]
  current_last_day_mrt <- dat_days_current[["last_day_mrt"]][1]
  dat_features_current <- cstress_featurevec %>% 
    filter(participant_id == current_id) %>% 
    filter((cstress_featurevec_hrts_local >= current_first_day_mrt) & (cstress_featurevec_hrts_local <= current_last_day_mrt))
  
  dates_database <- unique(lubridate::date(dat_features_current[["cstress_featurevec_hrts_local"]]))
  
  for(j in 1:nrow(dat_days_current)){
    this_date <- lubridate::date(dat_days_current[["date_local"]][j])
    count <- sum(this_date == dates_database)
    dat_days_current[j,"any_hr_data_today"] <- count
  }
  
  list_all <- append(list_all, list(dat_days_current))
}

dat_mrt_days <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# Create variables which capture engagement in data collection in various ways
# -----------------------------------------------------------------------------

all_ids <- unique(dat_mrt_days$participant_id)
dat_mrt_days$any_hr_data_after_day05 <- NA_real_
dat_mrt_days$any_hr_data_after_day07 <- NA_real_
dat_mrt_days$all_days_with_any_hr_data <- NA_real_
list_all <- list()

for(i in 1:length(all_ids)){
  current_id <- all_ids[i]
  dat_days_current <- dat_mrt_days %>% filter(participant_id == current_id)
  num_rows <- nrow(dat_days_current)
  
  if(num_rows > 4){
    subset_dat_days_current <- dat_days_current %>% filter(mrt_day > 5)
    is_exist_after_day05 <- 1 * (sum(subset_dat_days_current[["any_hr_data_today"]]) > 0)
    
    subset_dat_days_current <- dat_days_current %>% filter(mrt_day > 7)
    is_exist_after_day07 <- 1 * (sum(subset_dat_days_current[["any_hr_data_today"]]) > 0)
    
    is_exist_all_days <- 1 * (sum(dat_days_current[["any_hr_data_today"]]) == 11)
    
    dat_days_current <- dat_days_current %>% 
      mutate(any_hr_data_after_day05 = rep(is_exist_after_day05, num_rows),
             any_hr_data_after_day07 = rep(is_exist_after_day07, num_rows),
             all_days_with_any_hr_data = rep(is_exist_all_days, num_rows))
  }else{
    dat_days_current <- dat_days_current %>% 
      mutate(any_hr_data_after_day05 = rep(NA_real_, num_rows),
             any_hr_data_after_day07 = rep(NA_real_, num_rows),
             all_days_with_any_hr_data = rep(NA_real_, num_rows))
  }
  
  list_all <- append(list_all, list(dat_days_current))
}

dat_mrt_days <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# Merge visit attendance data with data on other indicators of engagement
# -----------------------------------------------------------------------------
dat_engagement_indicators <- left_join(x = dat_mrt_days, y = dat_visit_attendance, by = "participant_id")

# -----------------------------------------------------------------------------
# Save data
# -----------------------------------------------------------------------------
save(dat_engagement_indicators, file = file.path(path_staged_data, "dat_engagement_indicators.RData"))

# -----------------------------------------------------------------------------
# Visualize
# -----------------------------------------------------------------------------

plot(-1, -1, 
     xlim = c(0,10), ylim = c(1,50), 
     ylab = "Participant", xlab = "Day in MRT", cex.lab = 1.5,
     frame = "false", xaxt = "n", yaxt = "n")

for(i in 1:length(all_ids)){
  abline(h = i, col = "grey")
  
  current_id <- all_ids[i]
  dat_days_current <- dat_engagement_indicators %>% filter(participant_id == current_id)
  
  with_hr <- dat_days_current[["mrt_day"]][dat_days_current[["any_hr_data_today"]]==1]
  without_hr <- dat_days_current[["mrt_day"]][dat_days_current[["any_hr_data_today"]]==0]
  
  if(length(with_hr)>0){
    points(with_hr, rep(i, length(with_hr)), col = "darkgoldenrod", pch = 16, cex = 2)
  }
  
  if(length(without_hr)>0){
    points(without_hr, rep(i, length(without_hr)), col = "black", pch = 16, cex = 2)
  }
}

axis(1, at = 0:10, lwd = 5, cex.axis = 1.5)
axis(2, at = c(1, 10, 20, 30, 40, 49), lwd = 5, cex.axis = 1.5)

# -----------------------------------------------------------------------------
# Calculate summary statistics
# -----------------------------------------------------------------------------

# Number of participants having ANY heart rate data on Day 10
# ................................................. after Day 5
# ................................................. after Day 7
# Number of participants who completed their last clinic visit

dat_summary <- dat_engagement_indicators %>%
  filter(mrt_day == 10) %>%
  select(participant_id, 
         any_hr_data_today, 
         any_hr_data_after_day05, any_hr_data_after_day07, 
         all_days_with_any_hr_data,
         is_last_visit_completed)

dat_table <- dat_summary %>%
  summarise(tot_participants = n(),
            n_all_days = sum(all_days_with_any_hr_data),
            n_any_day10 = sum(any_hr_data_today),
            n_any_after_day05 = sum(any_hr_data_after_day05),
            n_any_after_day07 = sum(any_hr_data_after_day07),
            n_completed_last_visit = sum(is_last_visit_completed))

write.csv(dat_table, 
          "check-intermediate-datasets/collect-output/dat_summary_engagement.csv",
          row.names = FALSE)


