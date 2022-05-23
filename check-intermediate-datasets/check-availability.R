library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_matched_most_recent_classification.RData"))

dat <- dat_matched_most_recent_classification %>%
  mutate(elapsed_secs = as.numeric(difftime(time1 = rand_time_hrts_local, 
                                            time2 = when_most_recent_classification, 
                                            units = "secs"))) %>%
  mutate(elapsed_mins = elapsed_secs/60) %>%
  mutate(is_any_classification = !is.na(most_recent_classification)) %>%
  mutate(is_within = if_else(elapsed_mins <= 5, 1, 0))

# Count number of micro-randomizations by classification
dat_summary <- dat %>%
  group_by(most_recent_classification) %>%
  summarise(tot = n()) %>%
  arrange(desc(most_recent_classification)) %>%
  mutate(most_recent_classification = replace(most_recent_classification, is.na(most_recent_classification), "missing")) %>%
  add_row(most_recent_classification = "Total",
          tot = sum(.[["tot"]]))

write.csv(dat_summary, file.path("check-intermediate-datasets/collect-output/availability", "count_init_by_type.csv"), row.names = FALSE, na = "")

# Calculate median, 90th percentile and max number of seconds elapsed between most recent classification and micro-randomization
dat_summary <- dat %>%
  filter(is_any_classification) %>%
  filter(most_recent_classification != "active") %>%
  group_by(most_recent_classification) %>%
  summarise(tot = n(),
            q50 = quantile(elapsed_secs, 0.50),
            q90 = quantile(elapsed_secs, 0.90),
            q100 = max(elapsed_secs)) %>%
  arrange(desc(most_recent_classification)) %>%
  add_row(most_recent_classification = "Total",
          tot = sum(.[["tot"]]))

write.csv(dat_summary, file.path("check-intermediate-datasets/collect-output/availability", "quantiles_time_elapsed.csv"), row.names = FALSE, na = "")

# Count number of micro-randomizations which occur within 5 minutes of the most recent classification
dat_summary <- dat %>%
  filter(is_any_classification) %>%
  filter(most_recent_classification != "active") %>%
  group_by(most_recent_classification) %>%
  summarise(tot = n(),
            within = sum(is_within == 1),
            not_within = sum(is_within == 0)) %>%
  arrange(desc(most_recent_classification)) %>%
  add_row(most_recent_classification = "Total",
          tot = sum(.[["tot"]]))

write.csv(dat_summary, file.path("check-intermediate-datasets/collect-output/availability", "count_within_five.csv"), row.names = FALSE, na = "")

# Count number of micro-randomizations for which the probability of being randomized to any prompt is exactly zero or exactly one
dat_summary <- dat %>%
  filter(is_any_classification) %>%
  filter(most_recent_classification != "active") %>%
  filter(is_within==1) %>%
  group_by(most_recent_classification) %>%
  summarise(tot = n(),
            num_between = sum(probability > 0 & probability < 1),
            num_is_zero = sum(probability==0),
            num_is_one = sum(probability==1)) %>%
  arrange(desc(most_recent_classification)) %>%
  add_row(most_recent_classification = "Total",
          tot = sum(.[["tot"]]))

write.csv(dat_summary, file.path("check-intermediate-datasets/collect-output/availability", "count_violate_positivity.csv"), row.names = FALSE, na = "")


