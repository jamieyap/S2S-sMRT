library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_matched_most_recent_classification.RData"))

dat_matched_most_recent_classification <- dat_matched_most_recent_classification %>%
  mutate(elapsed_secs = as.numeric(difftime(time1 = rand_time_hrts_local, 
                                           time2 = when_most_recent_classification, 
                                           units = "secs")))

dat_summary <- dat_matched_most_recent_classification %>% 
  summarise(tot_rand = n(),
            tot_no_strata = sum(is.na(elapsed_secs)),
            tot_with_strata = sum(!is.na(elapsed_secs)),
            within_5min = sum(elapsed_secs <= 5*60, na.rm=TRUE),
            between_5_and_60min =  sum(elapsed_secs > 5*60 & elapsed_secs <= 60*60, na.rm=TRUE),
            more_than_60min = sum(elapsed_secs > 60*60, na.rm=TRUE)) %>%
  mutate(pct_within_5min = 100*(within_5min/tot_with_strata),
         pct_between_5_and_60min = 100*(between_5_and_60min/tot_with_strata),
         pct_more_than_60min = 100*(more_than_60min/tot_with_strata))

dat_summary_by_class <- dat_matched_most_recent_classification %>% 
  filter(!is.na(most_recent_classification)) %>%
  group_by(most_recent_classification) %>%
  summarise(tot_rand = n(),
            within_5min = sum(elapsed_secs <= 5*60, na.rm=TRUE),
            between_5_and_60min =  sum(elapsed_secs > 5*60 & elapsed_secs <= 60*60, na.rm=TRUE),
            more_than_60min = sum(elapsed_secs > 60*60, na.rm=TRUE)) %>%
  mutate(pct_within_5min = 100*(within_5min/tot_rand),
         pct_between_5_and_60min = 100*(between_5_and_60min/tot_rand),
         pct_more_than_60min = 100*(more_than_60min/tot_rand)) %>%
  add_row(most_recent_classification = "total",
          tot_rand = sum(.[["tot_rand"]]),
          within_5min = sum(.[["within_5min"]]),
          between_5_and_60min = sum(.[["between_5_and_60min"]]),
          more_than_60min = sum(.[["more_than_60min"]]),
          pct_within_5min = 100*(within_5min/tot_rand),
          pct_between_5_and_60min = 100*(between_5_and_60min/tot_rand),
          pct_more_than_60min = 100*(more_than_60min/tot_rand))


write.csv(dat_summary_by_class, file.path("check-intermediate-datasets", "collect-output", "summarize_stratification_time.csv"), row.names = FALSE)

dat_crosstab <- dat_matched_most_recent_classification %>%
  group_by(isStress, most_recent_classification, .groups = "keep") %>%
  summarise(count = n()) %>%
  select(isStress, most_recent_classification, count)

write.csv(dat_crosstab, file.path("check-intermediate-datasets", "collect-output", "dat_crosstab_strata.csv"), row.names = FALSE)

dat_crosstab_restricted <- dat_matched_most_recent_classification %>% 
  filter(!is.na(most_recent_classification)) %>%
  mutate(within_5min = if_else(elapsed_secs <= 5*60, 1, 0)) %>%
  filter(within_5min==1 & most_recent_classification!="active") %>%
  group_by(isStress, most_recent_classification, .groups = "keep") %>%
  summarise(count = n()) %>%
  select(isStress, most_recent_classification, count)

write.csv(dat_crosstab_restricted, file.path("check-intermediate-datasets", "collect-output", "dat_crosstab_restricted_strata.csv"), row.names = FALSE)

dat_restricted <- dat_matched_most_recent_classification %>% 
  filter(!is.na(most_recent_classification)) %>%
  mutate(within_5min = if_else(elapsed_secs <= 5*60, 1, 0)) %>%
  filter(within_5min==1 & most_recent_classification!="active") %>%
  mutate(condition01 = if_else(isStress==0 & most_recent_classification=="yes", 1, 0),
         condition02 = if_else(isStress==1 & most_recent_classification=="no", 1, 0)) %>%
  filter(condition01==1 | condition02==1)

dat_summary <- dat_restricted %>%
  summarise(minimum_seconds_between = min(elapsed_secs),
            maximum_seconds_between = max(elapsed_secs))

write.csv(dat_summary, file.path("check-intermediate-datasets", "collect-output", "summary_among_missclassed.csv"), row.names = FALSE)

tab1 <- dat_restricted %>%
  group_by(participant_id) %>%
  summarise(count_with_missclassed = n())

tab2 <- dat_matched_most_recent_classification %>%
  group_by(participant_id) %>%
  summarise(count_all = n())

tab3 <- left_join(x = tab2, y = tab1, by = "participant_id")  
tab3 <- tab3 %>%
  mutate(count_with_missclassed = if_else(is.na(count_with_missclassed), 0, as.double(count_with_missclassed))) %>%
  mutate(percent_missclassed = round(100*count_with_missclassed/count_all, 1)) %>%
  arrange(count_all) %>%
  mutate(id = 1:nrow(.)) %>%
  select(-participant_id) %>%
  select(id, everything())

write.csv(tab3, file.path("check-intermediate-datasets", "collect-output", "summary_missclassed_by_participant.csv"), row.names = FALSE)


