library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

load(file = file.path(path_staged_data, "parsed_dat_stress_episodes.RData"))
load(file = file.path(path_staged_data, "parsed_dat_activity.RData"))
load(file = file.path(path_staged_data, "cstress_featurevec.RData"))
load(file = file.path(path_staged_data, "dat_masterlist_updated.RData"))
load(file = file.path(path_staged_data, "skeleton.RData"))

dat_included_masterlist <- dat_masterlist %>%
  filter(is.na(exclude_reason)) %>%
  select(participant_id, first_day_mrt, last_day_mrt)

all_included_participants <- dat_included_masterlist$participant_id

dat_cleaned_episodes <- parsed_dat_stress_episodes %>%
  select(participant_id, episode_id,
         orig_episode_classification,
         episode_start_hrts_local, episode_peak_hrts_local, episode_end_hrts_local)

# When event==1, participant is classified as physically active
# When event==0, participant is classified as NOT physically active
dat_physically_active <- parsed_dat_activity %>% filter(event==1)

# -----------------------------------------------------------------------------
# Remove observations not within 'first day' and 'last day' of the MRT
# -----------------------------------------------------------------------------
dat_cleaned_episodes <- left_join(x = dat_cleaned_episodes, y = dat_included_masterlist, by = "participant_id")

dat_cleaned_episodes <- dat_cleaned_episodes %>%
  filter((episode_peak_hrts_local >= first_day_mrt) & (episode_peak_hrts_local <= last_day_mrt))

# -----------------------------------------------------------------------------
# Construct physical activity episodes
# -----------------------------------------------------------------------------
dat_cleaned_episodes$new_episode_classification <- dat_cleaned_episodes$orig_episode_classification
list_all <- list()

for(i in 1:length(all_included_participants)){
  smalldat_episodes <- dat_cleaned_episodes %>% filter(participant_id == all_included_participants[i])
  smalldat_activity <- dat_physically_active %>% filter(participant_id == all_included_participants[i])
  
  # Calculate the total number of physically active minutes between start and peak of an episode
  for(j in 1:nrow(smalldat_episodes)){
    current_start_time <- smalldat_episodes[j,"episode_start_hrts_local"]
    current_peak_time <- smalldat_episodes[j,"episode_peak_hrts_local"]
    current_class <- smalldat_episodes[j,"orig_episode_classification"]
    
    df_activity_within <- smalldat_activity %>% 
      filter((time_hrts_local>=current_start_time) & (time_hrts_local<=current_peak_time))
    
    # How many minutes between current_start_time and current_end_time 
    # were classified as physically active?
    count_minutes_within <- nrow(df_activity_within)
    # How many minutes are there between the start and peak of the episode?
    ab_len <- as.numeric(difftime(time1 = current_peak_time, time2 = current_start_time, units = "mins"))
    
    # If more then 50% of minutes between the start and peak of an unknown episode is physically active,
    # then regard the episode as physically active
    # else, still regard the episode as unknown
    if((count_minutes_within > ab_len/2) & (current_class == "unknown")){
      smalldat_episodes[j,"new_episode_classification"] <- "active"
    }
  }
  
  list_all <- append(list_all, list(smalldat_episodes))
}

dat_cleaned_episodes <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# For episodes (any type) for which the time elapsed between peak and end
# exceeds 5 minutes, determine whether any heart rate data exists between 
# A and B, and between B and C
# -----------------------------------------------------------------------------
newlist_all <- list()

# Subsequent calculations within the loop depend upon correct time-ordering of rows in cstress_featurevec
cstress_featurevec <- cstress_featurevec %>% arrange(participant_id, cstress_featurevec_unixts)
# Calculate minutes elapsed between peak and end of an episode
dat_cleaned_episodes <- dat_cleaned_episodes %>% 
  mutate(mins_elapsed_peak_and_end = as.numeric(difftime(time1 = episode_end_hrts_local, 
                                                         time2 = episode_peak_hrts_local, 
                                                         units = "mins"))) %>%
  mutate(is_exceeds_5min = if_else(mins_elapsed_peak_and_end > 5, 1, 0))
# Subsequent calculations within the loop depend upon unknown episodes having been removed from dat_cleaned_episodes
dat_cleaned_episodes <- dat_cleaned_episodes %>% filter(new_episode_classification!="unknown")

for(i in 1:length(all_included_participants)){
  smalldat_episodes <- dat_cleaned_episodes %>% filter(participant_id == all_included_participants[i])
  smalldat_featurevec <- cstress_featurevec %>% filter(participant_id == all_included_participants[i])
  
  for(j in 1:nrow(smalldat_episodes)){
    current_start_time <- smalldat_episodes[j,"episode_start_hrts_local"]
    current_peak_time <- smalldat_episodes[j,"episode_peak_hrts_local"]
    current_end_time <- smalldat_episodes[j,"episode_end_hrts_local"]
    
    # Check data between starting and peak of an episode
    featurevec_within_start_and_peak <- smalldat_featurevec %>% filter((cstress_featurevec_hrts_local >= current_start_time) & (cstress_featurevec_hrts_local <= current_peak_time))
    num_mins_within_start_and_peak <- nrow(featurevec_within_start_and_peak)
    smalldat_episodes[j,"is_exist_within_start_and_peak"] <- if_else(num_mins_within_start_and_peak==0, 0, 1)
    
    # Check data between peak and end of an episode
    featurevec_within_peak_and_end <- smalldat_featurevec %>% filter((cstress_featurevec_hrts_local >= current_peak_time) & (cstress_featurevec_hrts_local <= current_end_time))
    num_mins_within_peak_and_end <- nrow(featurevec_within_peak_and_end)
    smalldat_episodes[j,"is_exist_within_peak_and_end"] <- if_else(num_mins_within_peak_and_end==0, 0, 1)
  }
  
  newlist_all <- append(newlist_all, list(smalldat_episodes))
}

dat_cleaned_episodes_before_censoring <- do.call(rbind, newlist_all)

# Save intermediate output
save(dat_cleaned_episodes_before_censoring, file = file.path(path_staged_data, "dat_cleaned_episodes_before_censoring.RData"))

# -----------------------------------------------------------------------------
# Calculate summary statistics
# -----------------------------------------------------------------------------

summary0 <- dat_cleaned_episodes_before_censoring %>%
  filter(is_exceeds_5min==1) %>%
  group_by(is_exist_within_peak_and_end,
           .groups = "keep") %>%
  summarise(count = n()) %>%
  select(-c(".groups")) %>%
  arrange(desc(is_exist_within_peak_and_end))

summary0$percent <- (summary0$count/sum(summary0$count))*100
write.csv(summary0, file.path(path_staged_data, "summary0.csv"), row.names = FALSE)


summary1 <- dat_cleaned_episodes_before_censoring %>%
  filter(is_exceeds_5min==1) %>%
  group_by(is_exist_within_start_and_peak, 
           is_exist_within_peak_and_end,
           .groups = "keep") %>%
  summarise(count = n()) %>%
  select(-c(".groups")) %>%
  arrange(desc(is_exist_within_peak_and_end), desc(is_exist_within_start_and_peak))

summary1$percent <- (summary1$count/sum(summary1$count))*100
write.csv(summary1, file.path(path_staged_data, "summary1.csv"), row.names = FALSE)


summary2 <- dat_cleaned_episodes_before_censoring %>%
  filter(is_exceeds_5min==1) %>%
  group_by(new_episode_classification, 
           is_exist_within_start_and_peak, 
           is_exist_within_peak_and_end,
           .groups = "keep") %>%
  summarise(count = n()) %>%
  select(-c(".groups")) %>%
  arrange(desc(is_exist_within_peak_and_end), desc(is_exist_within_start_and_peak)) 

summary2$percent <- (summary2$count/sum(summary2$count))*100
write.csv(summary2, file.path(path_staged_data, "summary2.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# For episodes (any type) for which the time elapsed between peak and end
# exceeds 5 minutes, determine whether and how they should be censored
# -----------------------------------------------------------------------------
newlist_all <- list()
# Subsequent calculations within the loop depend upon correct time-ordering of rows in cstress_featurevec
cstress_featurevec <- cstress_featurevec %>% arrange(participant_id, cstress_featurevec_unixts)

for(i in 1:length(all_included_participants)){
  smalldat_episodes <- dat_cleaned_episodes_before_censoring %>% filter(participant_id == all_included_participants[i])
  smalldat_featurevec <- cstress_featurevec %>% filter(participant_id == all_included_participants[i])
  
  for(j in 1:nrow(smalldat_episodes)){
    current_start_time <- smalldat_episodes[j,"episode_start_hrts_local"]
    current_peak_time <- smalldat_episodes[j,"episode_peak_hrts_local"]
    current_end_time <- smalldat_episodes[j,"episode_end_hrts_local"]
    
    if(smalldat_episodes[j,"is_exceeds_5min"]==1 & smalldat_episodes[j,"is_exist_within_peak_and_end"]==1){
      featurevec_within_peak_and_end <- smalldat_featurevec %>% filter((cstress_featurevec_hrts_local >= current_peak_time) & (cstress_featurevec_hrts_local <= current_end_time))
      v <- c(current_peak_time, featurevec_within_peak_and_end[["cstress_featurevec_hrts_local"]], current_end_time)
      v_next <- c(featurevec_within_peak_and_end[["cstress_featurevec_hrts_local"]], current_end_time, NA)
      mins_between <- difftime(time1 = v_next, time2 = v, units = "mins")
      all_possible_idx <- which(mins_between > 5)
      if(length(all_possible_idx)>0){
        # Censor at the first time when there is 5 consecutive minutes of no heart rate data
        smalldat_episodes[j,"is_censored"] <- 1
        this_idx <- min(all_possible_idx)
        new_end_time <- v[this_idx]
        smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
        smalldat_episodes[j,"episode_newend_hrts_local"] <- new_end_time
      }else{
        smalldat_episodes[j,"is_censored"] <- 0
        smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
        smalldat_episodes[j,"episode_newend_hrts_local"] <- current_end_time
      }
    }else if(smalldat_episodes[j,"is_exceeds_5min"]==1 & smalldat_episodes[j,"is_exist_within_peak_and_end"]==0){
      # Censor at the peak (B)
      smalldat_episodes[j,"is_censored"] <- 1
      smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
      smalldat_episodes[j,"episode_newend_hrts_local"] <- current_peak_time
    }else{
      # This covers the case when an episode does NOT exceed 5 minutes; no censoring occurs
      smalldat_episodes[j,"is_censored"] <- 0
      smalldat_episodes[j,"episode_newstart_hrts_local"] <- current_start_time
      smalldat_episodes[j,"episode_newend_hrts_local"] <- current_end_time
    }
  }
  
  newlist_all <- append(newlist_all, list(smalldat_episodes))
}

dat_cleaned_episodes_after_censoring <- do.call(rbind, newlist_all)

# Save intermediate output
save(dat_cleaned_episodes_after_censoring, file = file.path(path_staged_data, "dat_cleaned_episodes_after_censoring.RData"))

# -----------------------------------------------------------------------------
# Calculate summary statistics
# -----------------------------------------------------------------------------

summary4 <- dat_cleaned_episodes_after_censoring %>%
  group_by(participant_id) %>%
  summarise(tot_episodes = n(),
            num_censored = sum(is_censored))

summary4$plot_id <- 1:nrow(summary4)

plot(-1, 
     xaxt = "n",
     yaxt = "n",
     xlim = c(0, nrow(summary4)), 
     ylim = c(0, max(summary4$tot_episodes)),
     xlab = "Each vertical line represents one participant",
     ylab = "Total No. of Episodes (green) vs. No. of Censored Episodes (blue)",
     frame = FALSE)
axis(2, lwd = 5, cex.axis = 1.5)
segments(x0 = summary4$plot_id, 
         x1 = summary4$plot_id, 
         y0 = rep(0,nrow(summary4)),
         y1 = summary4$tot_episodes,
         lwd = 20,
         col = "seagreen")
segments(x0 = summary4$plot_id, 
         x1 = summary4$plot_id, 
         y0 = rep(0,nrow(summary4)),
         y1 = summary4$num_censored,
         lwd = 20,
         col = "cornflowerblue")


