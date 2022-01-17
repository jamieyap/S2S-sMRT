library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "verified_dat_day_start.RData"))
load(file = file.path(path_staged_data, "skeleton.RData"))

verified_dat_day_start <- left_join(x = verified_dat_day_start,
                                    y = dat_mrt_days[, c("participant_id","date_local","mrt_day")],
                                    by = c("participant_id","date_local"))

verified_dat_day_start <- verified_dat_day_start %>% filter(!is.na(mrt_day))

dat <- verified_dat_day_start %>% 
  select(participant_id, day_start_time_hrts_local) %>%
  mutate(hour_of_day = hour(day_start_time_hrts_local) + (1/60)*minute(day_start_time_hrts_local) + (1/3600)*second(day_start_time_hrts_local))

all_ids <- unique(dat[["participant_id"]])

plot(0,0, 
     xlim = c(202, 270), 
     ylim = c(0,24), 
     xaxt = "n", yaxt = "n",
     xlab = "Data from the same participant\nare depicted by dots lying on the same dashed vertical line",
     ylab = "Time of day (0 = 12am, 12 = 12noon, 18 = 6pm)",
     frame = FALSE)

abline(h = 6, lwd = 10, col = "lavenderblush")
abline(h = 12, lwd = 10, col = "lavenderblush")
abline(h = 18, lwd = 10, col = "lavenderblush")

for(i in 1:length(all_ids)){
  segments(x0 = all_ids[i], y0 = 0, x1 = all_ids[i], y1 = 24, col = "grey87", lty = 2)
}

points(dat[["participant_id"]], dat[["hour_of_day"]], cex = 1.5, lwd = 1.2)
axis(2, at = c(0,3,6,9,12,15,18,21,24), lwd = 3)


