library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file.path(path_staged_data, "dat_rand.RData"))
load(file.path(path_staged_data, "dat_day_start.RData"))
load(file.path(path_staged_data, "dat_stress_episodes.RData"))
load(file.path(path_staged_data, "dat_activity.RData"))
load(file.path(path_staged_data, "dat_masterlist.RData"))

dat_stress_episodes %>%
  group_by(Stress_Episode_Classification) %>%
  summarise(count = n(),
            ave_duration = mean(AC_mins))

dat_stress_episodes %>%
  group_by(Stress_Episode_Classification) %>%
  summarise(count = n(),
            q10 = quantile(AC_mins, c(.10)),
            q50 = quantile(AC_mins, c(.50)),
            q75 = quantile(AC_mins, c(.75)),
            q90 = quantile(AC_mins, c(.90)))


