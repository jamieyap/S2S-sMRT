library(dplyr)
library(lubridate)
source("paths.R")

this_string <- "Sense2Stop REDCap_Consented Participants_Demographics Surveys Assessments_RAW.csv"
dat_baseline <- read.csv(file.path(path_raw_data_baseline, this_string))

dat_masterlist <- dat_baseline %>%
  filter(record_id >= 200) %>%
  select(record_id, day1_date) %>%
  mutate(participant_id = as.numeric(record_id),
         begin_study_hrts = mdy_hm(day1_date)) %>%
  mutate(begin_study_hrts = date(begin_study_hrts)) %>%
  mutate(quit_hrts = begin_study_hrts + days(3),
         end_study_hrts = begin_study_hrts + days(13)) %>%
  select(participant_id, begin_study_hrts, quit_hrts, end_study_hrts)

save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist.RData"))

