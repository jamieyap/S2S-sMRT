library(dplyr)
library(lubridate)
library(readxl)
source("paths.R")

dat_withdrawal <- read_xlsx(file.path(path_raw_data_study_staff, "Participants who withdrew.xlsx"),
                            col_types = c("numeric", "date"))

dat_withdrawal <- dat_withdrawal %>%
  mutate(participant_id = `Participant ID`,
         withdraw_date = `Withdraw Date`) %>%
  mutate(withdraw_date = as_date(withdraw_date))

dat_visit_tracking <- read_xlsx(file.path(path_raw_data_study_staff, "Sense2Stop Visit Tracking.xlsx"),
                                col_types = c("numeric", "date", "date", "date", "date", "date", "text"),
                                na = c("NA","N/A",""))

dat_visit_tracking <- dat_visit_tracking %>%
  mutate(`Study Start Date/Day 1 Visit` = as_date(`Study Start Date/Day 1 Visit`),
         `Expected Day 4/Post-Quit Visit Date` = as_date(`Expected Day 4/Post-Quit Visit Date`),
         `Actual Day 4/Post-Quit Visit Date` = as_date(`Actual Day 4/Post-Quit Visit Date`),
         `Day 14` = as_date(`Day 14`),
         `Equipment Return Visit/Day 15` = as_date(`Equipment Return Visit/Day 15`)) %>%
  rename(participant_id = `Participant ID`,
         begin_study_date = `Study Start Date/Day 1 Visit`,
         scheduled_visit_date = `Expected Day 4/Post-Quit Visit Date`,
         actual_visit_date = `Actual Day 4/Post-Quit Visit Date`,
         equipment_return_date = `Equipment Return Visit/Day 15`,
         notes = `Notes about Participant`) %>%
  # delayed_days: number of days elapsed between scheduled post-quit visit date 
  # and the date when the actual post-quit visit date occurred
  mutate(delayed_days = as.numeric(difftime(time1 = actual_visit_date, 
                                            time2 = scheduled_visit_date, 
                                            units = "days")))

dat_masterlist <- left_join(x = dat_visit_tracking, 
                            y = dat_withdrawal, 
                            by = "participant_id")

# Note that in dat_visit_tracking, all Day 14 dates were 
# consistently 13 days after the Day 1 visit
dat_masterlist <- dat_masterlist %>%
  mutate(end_study_date = if_else(!is.na(withdraw_date), 
                                  withdraw_date - days(1), 
                                  `Day 14`)) 

dat_masterlist <- dat_masterlist %>%
  mutate(exclude_reason = NA_character_) %>%
  mutate(exclude_reason = case_when(
    # C1: On or before the actual date of their 2nd lab visit, the participant 
    # informed study staff that they wish to withdraw
    (!is.na(withdraw_date)) & (is.na(actual_visit_date)) ~ "C1",
    # C2: Among those who were not counted in C1,
    # those participants who did not complete their 2nd lab visit
    (is.na(withdraw_date)) & (is.na(actual_visit_date)) ~ "C2",
    # C3: Were part of the trial's pilot run
    participant_id < 200 ~ "C3",
    TRUE ~ NA_character_
  ))

# Clean up columns
dat_masterlist <- dat_masterlist %>%
  select(participant_id, exclude_reason, withdraw_date,
         begin_study_date, 
         scheduled_visit_date, actual_visit_date, 
         delayed_days,
         end_study_date) %>%
  arrange(exclude_reason, desc(delayed_days), participant_id)

# Save output
save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist.RData"))


