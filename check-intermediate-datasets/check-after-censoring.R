library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "dat_cleaned_episodes_after_censoring.RData"))

# -----------------------------------------------------------------------------
# How many episodes were censored?
# -----------------------------------------------------------------------------

summarydat <- dat_cleaned_episodes_after_censoring %>%
  group_by(participant_id) %>%
  summarise(tot_episodes = n(),
            num_censored = sum(is_censored))

summarydat$plot_id <- 1:nrow(summarydat)

jpeg(file.path("check-intermediate-datasets", "collect-output", "viz_after_censor.jpg"),
     width = 14, height = 14, units = "in", res = 300)

plot(-1, 
     xaxt = "n",
     yaxt = "n",
     xlim = c(0, nrow(summarydat)), 
     ylim = c(0, max(summarydat$tot_episodes)),
     xlab = "Each vertical line represents one participant",
     ylab = "Total No. of Episodes (gray) vs. No. of Censored Episodes (blue)",
     main = paste("Note: Unknown episodes were not included in the counts.",
                  "Only probably stressed, probably not stressed, and physically active episodes were included in the counts",
                  sep="\n"),
     frame = FALSE)
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
# After censoring, what is the total length of time between A to C?
# -----------------------------------------------------------------------------

dat_cleaned_episodes_after_censoring <- dat_cleaned_episodes_after_censoring %>%
  mutate(AC_mins = as.numeric(difftime(time1 = episode_newend_hrts_local,
                                       time2 = episode_newstart_hrts_local,
                                       units = "mins"))) 

summarydat <- dat_cleaned_episodes_after_censoring %>%
  group_by(new_episode_classification) %>%
  summarise(q0 = quantile(AC_mins, probs = 0),
            q10 = quantile(AC_mins, probs = .10),
            q25 = quantile(AC_mins, probs = .25),
            q50 = quantile(AC_mins, probs = .50),
            q75 = quantile(AC_mins, probs = .75),
            q90 = quantile(AC_mins, probs = .90),
            q100 = quantile(AC_mins, probs = 1)) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    TRUE ~ NA_real_)) %>%
  arrange(my_order) %>%
  select(-my_order)

q90_yes <- summarydat[["q90"]][summarydat[["new_episode_classification"]] == "yes"]
q90_no <- summarydat[["q90"]][summarydat[["new_episode_classification"]] == "no"]
q90_active <- summarydat[["q90"]][summarydat[["new_episode_classification"]] == "active"]

summarydat[["q0"]] <- format(summarydat[["q0"]], nsmall=2, digits=2)
summarydat[["q10"]] <- format(summarydat[["q10"]], nsmall=2, digits=2)
summarydat[["q25"]] <- format(summarydat[["q25"]], nsmall=2, digits=2)
summarydat[["q50"]] <- format(summarydat[["q50"]], nsmall=2, digits=2)
summarydat[["q75"]] <- format(summarydat[["q75"]], nsmall=2, digits=2)
summarydat[["q90"]] <- format(summarydat[["q90"]], nsmall=2, digits=2)
summarydat[["q100"]] <- format(summarydat[["q100"]], nsmall=2, digits=2)

print(summarydat)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "censored_ACmins_percentiles.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Tabulate number of episodes which are M minutes long
# -----------------------------------------------------------------------------

summarydat <- dat_cleaned_episodes_after_censoring %>%
  group_by(new_episode_classification) %>%
  summarise(is_0min = sum(AC_mins==0),
            is_within_5min = sum(AC_mins>0 & AC_mins<=5),
            is_exceed_5min = sum(AC_mins>5)) %>%
  mutate(my_order = case_when(
    new_episode_classification == "yes" ~ 1,
    new_episode_classification == "no" ~ 2,
    new_episode_classification == "active" ~ 3,
    TRUE ~ NA_real_)) %>%
  arrange(my_order) %>%
  select(-my_order)

summarydat <- summarydat %>% 
  add_row(new_episode_classification = "total",
          is_0min = sum(.[["is_0min"]]),
          is_within_5min = sum(.[["is_within_5min"]]),
          is_exceed_5min = sum(.[["is_exceed_5min"]])) %>%
  mutate(total = is_0min + is_within_5min + is_exceed_5min)

print(summarydat)

write.csv(summarydat, file.path("check-intermediate-datasets", "collect-output", "censored_ACmins_counts.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Investigate those episodes for which length of time between A to C is zero
# -----------------------------------------------------------------------------

dat_cleaned_episodes_after_censoring <- dat_cleaned_episodes_after_censoring %>%
  mutate(mins_elapsed_start_and_peak = as.numeric(difftime(time1 = episode_peak_hrts_local,
                                                           time2 = episode_start_hrts_local,
                                                           units = "mins")))

dat_zeros <- dat_cleaned_episodes_after_censoring %>% filter(AC_mins == 0)
# Turns out that all of these episodes are such that their
# start and peak are identical, to the second-level
# This could potentially mean that they are less than 1 second in length
summary(dat_zeros$mins_elapsed_start_and_peak)

# -----------------------------------------------------------------------------
# Investigate the 90th to 100th percentiles
# -----------------------------------------------------------------------------

q90_yes <- as.numeric(q90_yes)
q90_no <- as.numeric(q90_no)
q90_active <- as.numeric(q90_active)

dat_tail <- dat_cleaned_episodes_after_censoring %>% 
  mutate(is_included = case_when(
    new_episode_classification=="yes" & AC_mins > q90_yes ~ 1,
    new_episode_classification=="no" & AC_mins > q90_no ~ 1,
    new_episode_classification=="active" & AC_mins > q90_active ~ 1,
    TRUE ~ 0
  )) %>%
  filter(is_included == 1) %>%
  arrange(desc(AC_mins))

# How many episodes?
print(nrow(dat_tail))

# What does the distribution of the tail look like?
fit_yes <- density(dat_tail[["AC_mins"]][dat_tail[["new_episode_classification"]] == "yes"], kernel = "gaussian")
fit_no <- density(dat_tail[["AC_mins"]][dat_tail[["new_episode_classification"]] == "no"], kernel = "gaussian")
fit_active <- density(dat_tail[["AC_mins"]][dat_tail[["new_episode_classification"]] == "active"], kernel = "gaussian")

# Plot estimated density
jpeg(file.path("check-intermediate-datasets", "collect-output", "ACmins_90percentile_and_above.jpg"),
     width = 7, height = 7, units = "in", res = 300)

xmin <- 0
xmax <- 2*60
these_vals <- seq(xmin, xmax, 1)
plot(-1, xlim = c(xmin, xmax), ylim = c(0, .20), xaxt = "n", yaxt = "n", frame = FALSE, 
     xlab = "",
     ylab = "")
lines(fit_yes, col = "tomato", lty = 2, type = "l", lwd = 3)
lines(fit_no, col = "cornflowerblue", lty = 2, type = "l", lwd = 3)
lines(fit_active, col = "seagreen", lty = 2, type = "l", lwd = 3)
axis(1, at = seq(xmin, xmax, 30), cex.axis = 1.5, lwd = 3)
axis(2, at = seq(0, .20, .05), cex.axis = 1.5, lwd = 3)
mtext("Density", side=2, line=3, cex=1.5)
mtext("Minutes between start and end of episode", side=1, line=3, cex=1.5)
legend("topright", 
       c("Probably Stressed",
         "Probably Not Stressed",
         "Physically Active"),
       lty = c(1,1,1),
       lwd = c(3,3,3),
       col = c("tomato","cornflowerblue","seagreen"),
       cex = 1.3)

dev.off()

