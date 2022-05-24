library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# -----------------------------------------------------------------------------
# Load relevant datasets
# -----------------------------------------------------------------------------
load(file.path(path_staged_data, "linked_dat.RData"))

# -----------------------------------------------------------------------------
# Which minutes will be regarded as eligible for micro-randomization?
# -----------------------------------------------------------------------------
dat <- linked_dat %>%
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
  )) %>% 
  select(-isStress)

k1 <- 0.05
k2 <- 0.95

dat <- dat %>%
  mutate(avail_raw = if_else(!is.na(trt_id), 1, 0)) %>%
  mutate(avail_aleph = case_when(
    is_any_classification == 0 ~ 0,
    most_recent_classification == "active" ~ 0,
    is_within == 0 ~ 0,
    probability == 0 ~ 0,
    probability == 1 ~ 0,
    TRUE  ~ 1
  )) %>%
  mutate(avail_omega = case_when(
    is_any_classification == 0 ~ 0,
    most_recent_classification == "active" ~ 0,
    is_within == 0 ~ 0,
    probability == 0 ~ 0,
    probability == 1 ~ 0,
    probability < k1 ~ 0,
    probability > k2 ~ 0,
    TRUE  ~ 1
  ))

# -----------------------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------------------

dat_percentiles <- dat %>%
  filter(!(avail_aleph == 0)) %>%
  group_by(avail_aleph, most_recent_classification, .groups = "keep") %>%
  summarise(tot = n(),
            q0 = quantile(probability, 0),
            q10 = quantile(probability, .10),
            q50 = quantile(probability, .50),
            q90 = quantile(probability, .90),
            q100 = quantile(probability, 1)) %>%
  select(-".groups")
  
dat_percentiles <- as.data.frame(dat_percentiles)
dat_percentiles <- dat_percentiles %>% add_row(tot = sum(.[["tot"]]))
write.csv(dat_percentiles, file.path("check-intermediate-datasets", "collect-output", "trim", "dat_prob_when_aleph_avail.csv"), row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------------------

dat_percentiles <- dat %>%
  filter(!(avail_omega == 0)) %>%
  group_by(avail_omega, most_recent_classification, .groups = "keep") %>%
  summarise(tot = n(),
            q0 = quantile(probability, 0),
            q10 = quantile(probability, .10),
            q50 = quantile(probability, .50),
            q90 = quantile(probability, .90),
            q100 = quantile(probability, 1)) %>%
  select(-".groups")

dat_percentiles <- as.data.frame(dat_percentiles)
dat_percentiles <- dat_percentiles %>% add_row(tot = sum(.[["tot"]]))
write.csv(dat_percentiles, file.path("check-intermediate-datasets", "collect-output", "trim", "dat_prob_when_omega_avail.csv"), row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------------------

dat_crosstab <- dat %>%
  group_by(avail_aleph, avail_omega, .groups = "keep") %>%
  summarise(tot = n()) %>%
  select(-".groups") %>%
  filter(!(avail_aleph == 0 & avail_omega == 0))

dat_crosstab <- as.data.frame(dat_crosstab)
dat_crosstab <- dat_crosstab %>% add_row(tot = sum(.[["tot"]]))

write.csv(dat_crosstab, file.path("check-intermediate-datasets", "collect-output", "trim", "dat_crosstab_avail.csv"), row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------------------

dat_crosstab <- dat %>%
  group_by(avail_aleph, avail_omega, most_recent_classification, .groups = "keep") %>%
  summarise(tot = n()) %>%
  select(-".groups") %>%
  filter(!(avail_aleph == 0 & avail_omega == 0))

dat_crosstab <- as.data.frame(dat_crosstab)
dat_crosstab <- dat_crosstab %>% add_row(tot = sum(.[["tot"]]))

write.csv(dat_crosstab, file.path("check-intermediate-datasets", "collect-output", "trim", "dat_crosstab_avail_by_class.csv"), row.names = FALSE, na = "")




