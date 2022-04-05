library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_matched_most_recent_classification.RData"))

dat_matched_most_recent_classification <- dat_matched_most_recent_classification %>%
  mutate(elapsed_min = as.numeric(difftime(time1 = rand_time_hrts_local, 
                                           time2 = when_most_recent_classification, 
                                           units = "secs")))

dat_summary <- dat_matched_most_recent_classification %>% 
  summarise(tot_rand = n(),
            tot_no_strata = sum(is.na(elapsed_min)),
            tot_with_strata = sum(!is.na(elapsed_min)),
            within_5min = sum(elapsed_min <= 5*60, na.rm=TRUE),
            between_5_and_60min =  sum(elapsed_min > 5*60 & elapsed_min <= 60*60, na.rm=TRUE),
            more_than_60min = sum(elapsed_min > 60*60, na.rm=TRUE)) %>%
  mutate(pct_within_5min = 100*(within_5min/tot_with_strata),
         pct_between_5_and_60min = 100*(between_5_and_60min/tot_with_strata),
         pct_more_than_60min = 100*(more_than_60min/tot_with_strata))

dat_summary_by_class <- dat_matched_most_recent_classification %>% 
  filter(!is.na(most_recent_classification)) %>%
  group_by(most_recent_classification) %>%
  summarise(tot_rand = n(),
            within_5min = sum(elapsed_min <= 5*60, na.rm=TRUE),
            between_5_and_60min =  sum(elapsed_min > 5*60 & elapsed_min <= 60*60, na.rm=TRUE),
            more_than_60min = sum(elapsed_min > 60*60, na.rm=TRUE)) %>%
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
  group_by(isStress, most_recent_classification) %>%
  summarise(count = n())

write.csv(dat_crosstab, file.path("check-intermediate-datasets", "collect-output", "dat_crosstab_strata.csv"), row.names = FALSE)

