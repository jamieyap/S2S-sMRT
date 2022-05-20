library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_cleaned_episodes_after_censoring.RData"))

# -----------------------------------------------------------------------------
# How many episodes were censored?
# Display summary statistics by episode type
# Only display summary statistics using episodes for which
# time between B and C which exceeds 5 minutes
# -----------------------------------------------------------------------------

summarydat <- dat_cleaned_episodes_after_censoring %>%
  filter(is_exceeds_5min==1) %>%
  group_by(new_episode_classification) %>%
  summarise(tot_episodes = n(),
            num_censored = sum(is_censored)) %>%
  mutate(percent_censored = round(100*num_censored/tot_episodes, 0))

summarydat$plot_id <- 1:nrow(summarydat)

jpeg(file.path("check-intermediate-datasets", "collect-output", "viz_after_censor_by_episode_type.jpg"),
     width = 10, height = 10, units = "in", res = 600)

plot(-1, 
     xaxt = "n",
     yaxt = "n",
     xlim = c(0, nrow(summarydat)+1), 
     ylim = c(0, max(summarydat$tot_episodes)),
     xlab = "",
     ylab = "",
     main = "Total No. of Episodes (gray) vs. No. of Censored Episodes (blue)",
     frame = FALSE,
     cex.main = 1.5)
axis(2, lwd = 5, cex.axis = 1.5)
text(x = 1:3,
     y = par("usr")[3] - 0.15,
     labels = c("Physically Active", "Probably Not Stressed", "Probably Stressed"),
     xpd = NA,
     srt = 20,
     adj = 0.9,
     cex = 1.5)
segments(x0 = summarydat$plot_id, 
         x1 = summarydat$plot_id, 
         y0 = rep(0,nrow(summarydat)),
         y1 = summarydat$tot_episodes,
         lwd = 20,
         col = "gray")
segments(x0 = summarydat$plot_id, 
         x1 = summarydat$plot_id, 
         y0 = rep(0,nrow(summarydat)),
         y1 = summarydat$num_censored,
         lwd = 20,
         col = "cornflowerblue")
text(x = 1:3 - 0.25, y = c(1500, 2700, 700), 
     c(paste(summarydat[["percent_censored"]][1], "%", sep=""), 
       paste(summarydat[["percent_censored"]][2], "%", sep=""), 
       paste(summarydat[["percent_censored"]][3], "%", sep="")), 
     col = "skyblue4", cex = 2)

dev.off()

# -----------------------------------------------------------------------------
# How many episodes were censored?
# Only display summary statistics using episodes for which 
# time between B and C exceeds 5 minutes
# -----------------------------------------------------------------------------

summarydat <- dat_cleaned_episodes_after_censoring %>%
  filter(is_exceeds_5min==1) %>%
  group_by(participant_id) %>%
  summarise(tot_episodes = n(),
            num_censored = sum(is_censored)) %>%
  mutate(percent_censored = round(100*num_censored/tot_episodes, 0)) %>%
  arrange(desc(tot_episodes))

summarydat$plot_id <- 1:nrow(summarydat)

jpeg(file.path("check-intermediate-datasets", "collect-output", "viz_after_censor_by_participant.jpg"),
     width = 14, height = 14, units = "in", res = 600)

plot(-1, 
     xaxt = "n",
     yaxt = "n",
     xlim = c(0, nrow(summarydat)), 
     ylim = c(0, max(summarydat$tot_episodes)),
     xlab = "Each vertical line represents one participant",
     # All types of episodes (probably stressed, probably not stressed, physically active) were included in this plot
     ylab = "",
     main = "Total No. of Episodes (gray) vs. No. of Censored Episodes (blue)",
     frame = FALSE,
     cex.main = 1.5)
axis(2, lwd = 5, cex.axis = 1.5)
axis(1, at = c(1, 7*c(1,2,3,4,5,6,7)), labels = c(1, 7*c(1,2,3,4,5,6,7)), tick = TRUE, lwd = 0, lwd.ticks = 0, cex.axis = 1.5)
segments(x0 = summarydat$plot_id, 
         x1 = summarydat$plot_id, 
         y0 = rep(0,nrow(summarydat)),
         y1 = summarydat$tot_episodes,
         lwd = 20,
         col = "gray")
segments(x0 = summarydat$plot_id, 
         x1 = summarydat$plot_id, 
         y0 = rep(0,nrow(summarydat)),
         y1 = summarydat$num_censored,
         lwd = 20,
         col = "cornflowerblue")
dev.off()

# -----------------------------------------------------------------------------
# After censoring, what is the total length of time between B to C?
# Display summary statistics on the aggregate
# -----------------------------------------------------------------------------

summarydat <- dat_cleaned_episodes_after_censoring %>%
  filter(is_exceeds_5min==1) %>%
  mutate(BC_mins = as.numeric(difftime(time1 = episode_newend_hrts_local, 
                                       time2 = episode_newpeak_hrts_local, 
                                       units = "mins"))) %>%
  group_by(new_episode_classification) %>%
  summarise(tot = n(),
            q50 = quantile(BC_mins, probs = .50),
            q90 = quantile(BC_mins, probs = .90),
            q100 = quantile(BC_mins, probs = 1)) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    TRUE ~ NA_real_))  %>%
  arrange(my_order) %>%
  select(-my_order) %>%
  add_row(new_episode_classification = "tot",
          tot = sum(.[["tot"]]),
          q50 = NA_real_,
          q90 = NA_real_,
          q100 = NA_real_)

colnames(summarydat) <- c("Episode Type", 
                          "No. of episodes for which the duration of time between B and C is greater than 5 minutes", 
                          "Median time (in minutes) between B and C", 
                          "90th percentile time (in minutes) between B and C",
                          "Max time (in minutes) between B and C")
print(summarydat)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "BCmins_after_censoring.csv"), row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# After censoring, what is the total length of time between A to C?
# -----------------------------------------------------------------------------

summarydat <- dat_cleaned_episodes_after_censoring %>%
  mutate(AC_mins = as.numeric(difftime(time1 = episode_newend_hrts_local, 
                                       time2 = episode_newstart_hrts_local, 
                                       units = "mins"))) %>%
  group_by(new_episode_classification) %>%
  summarise(tot = n(),
            q50 = quantile(AC_mins, probs = .50),
            q90 = quantile(AC_mins, probs = .90),
            q100 = quantile(AC_mins, probs = 1)) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    TRUE ~ NA_real_))  %>%
  arrange(my_order) %>%
  select(-my_order) %>%
  add_row(new_episode_classification = "tot",
          tot = sum(.[["tot"]]),
          q50 = NA_real_,
          q90 = NA_real_,
          q100 = NA_real_)

colnames(summarydat) <- c("Episode Type", 
                          "No. of episodes", 
                          "Median time (in minutes) between A and C", 
                          "90th percentile time (in minutes) between A and C",
                          "Max time (in minutes) between A and C")
print(summarydat)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "ACmins_after_censoring.csv"), row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# After censoring, what is the total length of time between A to C?
# -----------------------------------------------------------------------------

summarydat <- dat_cleaned_episodes_after_censoring %>%
  mutate(AC_mins = as.numeric(difftime(time1 = episode_newend_hrts_local, 
                                       time2 = episode_newstart_hrts_local, 
                                       units = "mins"))) %>%
  summarise(tot = n(),
            q50 = quantile(AC_mins, probs = .50),
            q90 = quantile(AC_mins, probs = .90),
            q100 = quantile(AC_mins, probs = 1)) 

colnames(summarydat) <- c("No. of episodes", 
                          "Median time (in minutes) between A and C", 
                          "90th percentile time (in minutes) between A and C",
                          "Max time (in minutes) between A and C")
print(summarydat)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "ACmins_after_censoring_aggregate.csv"), row.names = FALSE, na = "")


