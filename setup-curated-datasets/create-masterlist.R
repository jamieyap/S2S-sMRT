library(dplyr)
library(lubridate)
library(readxl)
source("paths.R")

# -----------------------------------------------------------------------------
# Read in file created by study staff and rename columns
# -----------------------------------------------------------------------------

dat_visit_tracking <- read_xlsx(file.path(path_raw_data_study_staff, "Sense2Stop_Participant_Dates.xlsx"),
                                col_types = c("numeric", "date", "date", "date", "date", "date", "numeric", "date", "text", "text"),
                                na = c(""))
# After reading in the excel file, "date" column types will be of variable type POSIXct
# Calling the function as_date() will coerce these columns to be of variable type Date

dat_visit_tracking <- dat_visit_tracking %>%
  mutate(`Study Start Date/1st Visit` = as_date(`Study Start Date/1st Visit`),
         `Scheduled 2nd Visit Date` = as_date(`Scheduled 2nd Visit Date`),
         `Completed 2nd Visit Date` = as_date(`Completed 2nd Visit Date`),
         `Scheduled Last Visit` = as_date(`Scheduled Last Visit`),
         `Completed Last Visit` = as_date(`Completed Last Visit`),
         `Withdraw Date` = as_date(`Withdraw Date`)) %>%
  rename(participant_id = `Participant ID`,
         begin_study_date = `Study Start Date/1st Visit`,
         scheduled_visit_date = `Scheduled 2nd Visit Date`,
         actual_visit_date = `Completed 2nd Visit Date`,
         scheduled_equipment_return_date = `Scheduled Last Visit`,
         actual_equipment_return_date = `Completed Last Visit`,
         withdraw_status = `Withdraw Status`,
         withdraw_date = `Withdraw Date`,
         withdraw_reason = `Reason for Withdraw`,
         other_notes = `Other Notes`)

# -----------------------------------------------------------------------------
# Create two variables to represent the 'first day' and 'last day' of the MRT
# Note that first_day_mrt coincides with Quit Date
# -----------------------------------------------------------------------------

dat_masterlist <- dat_visit_tracking

# Day 0 (i.e., 'first day' of the MRT) is set to the date when an individual
# completes their 2nd lab visit
dat_masterlist <- dat_masterlist %>%
  mutate(first_day_mrt = actual_visit_date) %>%
  mutate(last_day_mrt = first_day_mrt + days(10) + days(1)) %>%
  # Note: The next step will make first_day_mrt and last_day_mrt to be
  # at 12am America/Chicago time on the same calendar date
  mutate(first_day_mrt = force_tz(as_datetime(first_day_mrt), tz = "America/Chicago")) %>%
  mutate(last_day_mrt = force_tz(as_datetime(last_day_mrt), tz = "America/Chicago")) %>%
  mutate(last_day_mrt = replace(last_day_mrt, is.na(first_day_mrt), as_datetime(NA)))

# -----------------------------------------------------------------------------
# Among participants who formally withdrew, identify participants for which
# withdraw date comes after actual 2nd lab visit date;
# for these participants, set last_day_mrt to be the day prior to when
# they formally withdrew
# -----------------------------------------------------------------------------

dat_masterlist <- dat_masterlist %>%
  mutate(withdraw_days_after_quit = as.numeric(difftime(time1 = withdraw_date, 
                                                        time2 = actual_visit_date, 
                                                        units = "days"))) %>%
  mutate(last_day_mrt = if_else(!is.na(withdraw_days_after_quit) & (withdraw_days_after_quit > 0), 
                                first_day_mrt + days(withdraw_days_after_quit) - days(1), 
                                last_day_mrt))

# -----------------------------------------------------------------------------
# Identify participants for which actual and scheduled 2nd lab visit date
# do not coincide
# -----------------------------------------------------------------------------

dat_masterlist <- dat_masterlist %>%
  mutate(delayed_days = as.numeric(difftime(time1 = actual_visit_date, 
                                            time2 = scheduled_visit_date, 
                                            units = "days"))) 

# -----------------------------------------------------------------------------
# Create a variable to record why a participant will not be included in 
# the data for analysis
# -----------------------------------------------------------------------------

dat_masterlist <- dat_masterlist %>%
  mutate(exclude_reason = NA_character_) %>%
  mutate(exclude_reason = case_when(
    # C1: On or before the actual date of their 2nd lab visit, the participant 
    # informed study staff that they wish to withdraw
    # Note: this case does NOT include the 1 participant who formally withdrew 
    # 5 days AFTER completing their 2nd lab visit
    (withdraw_status==1) & (is.na(actual_visit_date)) ~ "C1",
    # C2: Among those who were not counted in C1,
    # those participants who did not complete their 2nd lab visit
    (withdraw_status==0) & (is.na(actual_visit_date)) ~ "C2",
    # C3: Were part of the trial's pilot run
    participant_id < 200 ~ "C3",
    TRUE ~ NA_character_
  ))

# -----------------------------------------------------------------------------
# Clean up columns
# -----------------------------------------------------------------------------

dat_masterlist <- dat_masterlist %>%
  arrange(exclude_reason, desc(delayed_days), desc(withdraw_days_after_quit), participant_id)

dat_masterlist <- dat_masterlist %>%
  select(participant_id, exclude_reason, withdraw_status, 
         begin_study_date, first_day_mrt, last_day_mrt, 
         delayed_days, withdraw_days_after_quit,
         everything())

# -----------------------------------------------------------------------------
# Inspect elements
# -----------------------------------------------------------------------------
dat_masterlist %>% filter(delayed_days > 0)

dat_masterlist %>% filter(withdraw_days_after_quit > 0)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

dat_masterlist <- dat_masterlist %>%
  select(participant_id, exclude_reason, withdraw_status, 
         begin_study_date, first_day_mrt, last_day_mrt)

save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist.RData"))

