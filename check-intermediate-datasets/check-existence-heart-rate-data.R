library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

# Note that dat_heart_rate_indicator does not include unknown episodes
# That is, it only includes probably stressed, probably not stressed, and 
# physically active episodes
load(file = file.path(path_staged_data, "dat_heart_rate_indicator.RData"))

# -----------------------------------------------------------------------------
# In how many episodes does heart rate data exist 
# ... between start and peak
# ... between peak and end
# Present counts in the aggregate
# -----------------------------------------------------------------------------

summary1 <- dat_heart_rate_indicator %>%
  filter(is_exceeds_5min==1) %>%
  group_by(is_exist_within_start_and_peak, 
           is_exist_within_peak_and_end,
           .groups = "keep") %>%
  summarise(count = n()) %>%
  select(-c(".groups")) %>%
  arrange(desc(is_exist_within_peak_and_end), desc(is_exist_within_start_and_peak))

summary1[["percent"]]<- summary1[["count"]]/sum(summary1[["count"]])*100
summary1 <- rbind(summary1,
                  c(is_exist_within_start_and_peak = NA, 
                    is_exist_within_peak_and_end = NA, 
                    count = sum(summary1[["count"]]), 
                    percent = sum(summary1[["percent"]])))

summary1[["percent"]] <- format(summary1[["percent"]], digits=2, nsmall=2)

write.csv(summary1, file.path(path_staged_data, "count_any_heart_rate_aggregate.csv"), row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# In how many episodes does heart rate data exist 
# ... between start and peak
# ... between peak and end
# Disaggregate counts according to episode type
# -----------------------------------------------------------------------------

summary2 <- dat_heart_rate_indicator %>%
  filter(is_exceeds_5min==1) %>%
  group_by(new_episode_classification, 
           is_exist_within_start_and_peak, 
           is_exist_within_peak_and_end,
           .groups = "keep") %>%
  summarise(count = n()) %>%
  select(-c(".groups")) %>%
  arrange(desc(is_exist_within_peak_and_end), desc(is_exist_within_start_and_peak)) 

summary2[["percent"]]<- summary2[["count"]]/sum(summary2[["count"]])*100
summary2 <- rbind(summary2,
                  c(is_exist_within_start_and_peak = NA, 
                    is_exist_within_peak_and_end = NA, 
                    count = sum(summary2[["count"]]), 
                    percent = sum(summary2[["percent"]])))

summary2[["percent"]] <- format(summary2[["percent"]], digits=2, nsmall=2)

write.csv(summary2, file.path(path_staged_data, "count_any_heart_rate_disaggregate.csv"), row.names = FALSE, na = "")


