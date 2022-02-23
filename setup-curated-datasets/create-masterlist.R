library(dplyr)
library(lubridate)
library(readxl)
source("paths.R")

dat_visit_tracking <- read_xlsx(file.path(path_raw_data_study_staff, "Sense2Stop_Participant_Dates.xlsx"),
                                col_types = c("numeric", "date", "date", "date", "date", "date", "numeric", "date", "text", "text"),
                                na = c(""))

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

dat_visit_tracking <- dat_visit_tracking %>%
  # delayed_days: number of days elapsed between scheduled post-quit visit date 
  # and the date when the actual post-quit visit date occurred
  mutate(delayed_days = as.numeric(difftime(time1 = actual_visit_date, 
                                            time2 = scheduled_visit_date, 
                                            units = "days"))) %>%
  mutate(withdraw_days_after_quit = as.numeric(difftime(time1 = withdraw_date, 
                                                        time2 = actual_visit_date, 
                                                        units = "days")))

dat_visit_tracking <- dat_visit_tracking %>%
  mutate(end_study_date = case_when(
    # Participants who formally withdrew and did not complete 2nd lab visit
    withdraw_status==1 & is.na(withdraw_days_after_quit) ~ as_date(NA),
    # Participants who formally withdrew but completed the 2nd lab visit
    # Note that this case only include 1 participant and this participant withdrew 5 days AFTER completing their 2nd lab visit 
    withdraw_status==1 & !is.na(withdraw_days_after_quit) & (withdraw_days_after_quit>0) ~ withdraw_date-days(1),
    # Participants who did not formally withdraw and who completed their 2nd lab visit
    (withdraw_status==0) & (!is.na(actual_visit_date)) ~ actual_visit_date + days(10),
    # All other cases; includes participants who did not formally withdraw but did not complete their 2nd lab visit
    TRUE ~ as_date(NA)
  ))

dat_masterlist <- dat_visit_tracking %>%
  mutate(exclude_reason = NA_character_) %>%
  mutate(exclude_reason = case_when(
    # C1: On or before the actual date of their 2nd lab visit, the participant 
    # informed study staff that they wish to withdraw
    # Note: this case does NOT include the 1 participant who formally withdrew 5 days AFTER completing their 2nd lab visit
    (withdraw_status==1) & (is.na(actual_visit_date)) ~ "C1",
    # C2: Among those who were not counted in C1,
    # those participants who did not complete their 2nd lab visit
    (withdraw_status==0) & (is.na(actual_visit_date)) ~ "C2",
    # C3: Were part of the trial's pilot run
    participant_id < 200 ~ "C3",
    TRUE ~ NA_character_
  ))
  
# Clean up columns
dat_masterlist <- dat_masterlist %>%
  select(participant_id, exclude_reason, withdraw_status, withdraw_date, everything()) %>%
  arrange(exclude_reason, desc(delayed_days), participant_id)

# Save output
save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist.RData"))


