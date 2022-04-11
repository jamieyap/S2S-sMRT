library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "data_for_analysis.RData"))

# -----------------------------------------------------------------------------
# Using all eligible decision points, calculate proportion missing when
# the value of the stratification variable was 'probably stress' (red)
# vs. 'probably not stressed' (blue)
# -----------------------------------------------------------------------------
jpeg(file.path("check-intermediate-datasets", "collect-output", "viz_proportion_missing.jpg"),
     width = 14, height = 10, units = "in", res = 600)

dat_summary <- data_for_analysis %>%
  filter(most_recent_classification == "no") %>%
  select(starts_with("Y"))
dat_summary <- is.na(dat_summary)
dat_summary <- colMeans(dat_summary)
total_look_ahead <- length(dat_summary)

num_dp_no <- data_for_analysis %>%
  filter(most_recent_classification == "no") %>%
  nrow(.)

plot(-1, 
     xaxt = "n",
     yaxt = "n",
     xlim = c(0, total_look_ahead), 
     ylim = c(0, 1),
     xlab = "No. of mins after micro-randomization at an eligible decision point",
     ylab = "Proportion of decision points with missing value in trichotomous variable",
     main = "",
     frame = FALSE)

axis(2, lwd = 5, cex.axis = 1.5, cex.lab = 1.5)
axis(1, at = c(1, seq(10, total_look_ahead, 10)), 
     labels = c(1, seq(10, total_look_ahead, 10)), 
     tick = TRUE, lwd = 5, lwd.ticks = 5, cex.axis = 1.5, cex.lab = 1.5)

points(x = seq_len(total_look_ahead),
       y = dat_summary,
       pch = 21,
       cex = 2,
       bg = "cornflowerblue")

dat_summary <- data_for_analysis %>%
  filter(most_recent_classification == "yes") %>%
  select(starts_with("Y"))
dat_summary <- is.na(dat_summary)
dat_summary <- colMeans(dat_summary)
total_look_ahead <- length(dat_summary)

num_dp_yes <- data_for_analysis %>%
  filter(most_recent_classification == "yes") %>%
  nrow(.)

points(x = seq_len(total_look_ahead),
       y = dat_summary,
       pch = 21,
       cex = 2,
       bg = "tomato")

legend(60, 0.2, 
       c("Micro-randomization while probably stressed",
         "Micro-randomization while probably not stressed"),
       pch = c(21, 21), 
       col = c("black","black"),
       pt.bg = c("tomato","cornflowerblue"),
       pt.cex = 2,
       cex = 1.5)

title(paste(num_dp_yes, "micro-randomizations at eligible decision points while probably stressed",
            "\n",
            num_dp_no, "micro-randomizations at eligible decision points while probably not stressed",
            sep = " "),
      cex.main = 1.5)

dev.off()
