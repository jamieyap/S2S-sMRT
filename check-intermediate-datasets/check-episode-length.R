library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_full_episodes.RData"))

# -----------------------------------------------------------------------------
# Number of episodes by type
# -----------------------------------------------------------------------------

summarydat <- dat_full_episodes %>%
  mutate(BC_mins = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                       time2 = episode_peak_hrts_local, 
                                       units = "mins"))) %>%
  group_by(new_episode_classification) %>%
  summarise(tot = n()) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    new_episode_classification == "unknown" ~ 4,
    TRUE ~ NA_real_)) %>%
  arrange(my_order) %>%
  select(-my_order) %>%
  add_row(new_episode_classification = "tot",
          tot = sum(.[["tot"]]))

grand_total <- summarydat[["tot"]][summarydat[["new_episode_classification"]] == "tot"]

summarydat <- summarydat %>%
  mutate(percent = round(100 * (tot/grand_total), digits = 1))

colnames(summarydat) <- c("Episode Type", "No. of Episodes", "Percent of Total")

print(summarydat)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "count_by_episode_type.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Number of episodes for which the number of minutes elapsed between B and C is 
# greater than 5 minutes
# -----------------------------------------------------------------------------

summarydat <- dat_full_episodes %>%
  mutate(BC_mins = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                       time2 = episode_peak_hrts_local, 
                                       units = "mins"))) %>%
  group_by(new_episode_classification) %>%
  summarise(cnt5 = sum(BC_mins>5),
            q50 = quantile(BC_mins, probs = .50),
            q90 = quantile(BC_mins, probs = .90),
            q100 = quantile(BC_mins, probs = 1)) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    new_episode_classification == "unknown" ~ 4,
    TRUE ~ NA_real_))  %>%
  filter(new_episode_classification != "unknown") %>%
  arrange(my_order) %>%
  select(-my_order) %>%
  add_row(new_episode_classification = "tot",
          cnt5 = sum(.[["cnt5"]]),
          q50 = NA_real_,
          q90 = NA_real_,
          q100 = NA_real_)

colnames(summarydat) <- c("Episode Type", 
                          "No. of episodes for which the duration of time between B and C is greater than 5 minutes", 
                          "Median time (in minutes) between B and C", 
                          "90th percentile time (in minutes) between B and C",
                          "Max time (in minutes) between B and C")
print(summarydat)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "BCmins_before_censoring.csv"), row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# # What does the distribution of the duration of time between B and C 
# before censoring look like?
# -----------------------------------------------------------------------------

dat_full_episodes <- dat_full_episodes %>%
  mutate(BC_mins = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                       time2 = episode_peak_hrts_local, 
                                       units = "mins"))) 

fit_yes <- density(dat_full_episodes[["BC_mins"]][dat_full_episodes[["new_episode_classification"]] == "yes"], kernel = "gaussian")
fit_no <- density(dat_full_episodes[["BC_mins"]][dat_full_episodes[["new_episode_classification"]] == "no"], kernel = "gaussian")
fit_active <- density(dat_full_episodes[["BC_mins"]][dat_full_episodes[["new_episode_classification"]] == "active"], kernel = "gaussian")

# Plot estimated density
jpeg(file.path("check-intermediate-datasets", "collect-output", "BCmins_before_censoring.jpg"),
     width = 10, height = 10, units = "in", res = 600)

xmin <- 0
xmax <- 30 #960
these_vals <- seq(xmin, xmax, 5)
plot(-1, xlim = c(xmin, xmax), ylim = c(0, .20), xaxt = "n", yaxt = "n", frame = FALSE, 
     xlab = "",
     ylab = "")
lines(fit_yes, col = "tomato", lty = 2, type = "l", lwd = 3)
lines(fit_no, col = "cornflowerblue", lty = 2, type = "l", lwd = 3)
lines(fit_active, col = "seagreen", lty = 2, type = "l", lwd = 3)
axis(1, at = these_vals, cex.axis = 1.5, lwd = 3)
axis(2, at = seq(0, .20, .05), cex.axis = 1.5, lwd = 3)
mtext("Density", side=2, line=3, cex=1.5)
mtext("Minutes between B and C before censoring", side=1, line=3, cex=1.5)
legend("topright", 
       c("Probably Stressed",
         "Probably Not Stressed",
         "Physically Active"),
       lty = c(1,1,1),
       lwd = c(3,3,3),
       col = c("tomato","cornflowerblue","seagreen"),
       cex = 1.3)

dev.off()


