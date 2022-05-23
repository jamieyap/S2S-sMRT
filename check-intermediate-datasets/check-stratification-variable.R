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
  mutate(is_within = if_else(elapsed_mins <= 5, 1, 0)) %>%
  mutate(case_indicator = case_when(
    isStress==1 & most_recent_classification=="yes" ~ 1,
    isStress==0 & most_recent_classification=="no" ~ 2,
    isStress==1 & most_recent_classification=="no" ~ 3,
    isStress==0 & most_recent_classification=="yes" ~ 4,
    TRUE ~ NA_real_
  ))

dat_restricted <- dat %>% 
  filter(is_any_classification) %>%
  filter(most_recent_classification != "active") %>%
  filter(is_within == 1) %>%
  filter((probability != 0) & (probability != 1))

# -----------------------------------------------------------------------------
# Compare two data sources: isStress and most_recent_classification
# -----------------------------------------------------------------------------

dat_crosstab <- dat_restricted %>%
  group_by(isStress, most_recent_classification, .groups = "keep") %>%
  summarise(tot = n(),
            min_secs = min(elapsed_secs),
            max_secs = max(elapsed_secs))  %>%
  mutate(case_indicator = case_when(
    isStress==1 & most_recent_classification=="yes" ~ 1,
    isStress==0 & most_recent_classification=="no" ~ 2,
    isStress==1 & most_recent_classification=="no" ~ 3,
    isStress==0 & most_recent_classification=="yes" ~ 4,
    TRUE ~ NA_real_
  )) %>%
  arrange(case_indicator) %>%
  select(-".groups", -"case_indicator")

write.csv(dat_crosstab, file.path("check-intermediate-datasets", "collect-output", "stratification", "dat_crosstab_strata.csv"), row.names = FALSE)

