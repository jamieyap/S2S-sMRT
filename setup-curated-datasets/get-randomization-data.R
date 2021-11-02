library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

# -----------------------------------------------------------------------------
# We begin with log files for each participant, which have already been 
# pre-extracted from zip folders
# -----------------------------------------------------------------------------

all_file_names <- list.files(path = file.path(path_processed_data, "stress_cleaned_data"))

phone_log_backup_file_names <- grepl(pattern = "phone_log_backup", x = all_file_names)
phone_log_backup_file_names <- all_file_names[phone_log_backup_file_names]
ids_present_backup <- substr(x = phone_log_backup_file_names, start = 17, stop = 19)
ids_present_backup <- as.numeric(ids_present_backup)

phone_log_cloud_file_names <- grepl(pattern = "phone_log_original", x = all_file_names)
phone_log_cloud_file_names <- all_file_names[phone_log_cloud_file_names]
ids_present_cloud <- substr(x = phone_log_cloud_file_names, start = 19, stop = 21)
ids_present_cloud <- as.numeric(ids_present_cloud)

phone_log_alternative_file_names <- grepl(pattern = "phone_log_alternative", x = all_file_names)
phone_log_alternative_file_names <- all_file_names[phone_log_alternative_file_names]
ids_present_alternative <- substr(x = phone_log_alternative_file_names, start = 22, stop = 24)
ids_present_alternative <- as.numeric(ids_present_alternative)

list_dat_all <- list()

scan_these_ids <- ids_present_backup
scan_these_file_names <- paste("phone_log_backup", scan_these_ids, ".csv", sep="")

for(i in 1:length(scan_these_ids)){
  curr_file_name <- scan_these_file_names[i]
  curr_file <- read.csv(file.path(path_processed_data, "stress_cleaned_data", curr_file_name))
  list_dat_all <- append(list_dat_all, list(curr_file))
}

scan_these_ids <- setdiff(x = ids_present_cloud, y = ids_present_backup)
scan_these_file_names <- paste("phone_log_original", scan_these_ids, ".csv", sep="")

for(i in 1:length(scan_these_ids)){
  curr_file_name <- scan_these_file_names[i]
  curr_file <- read.csv(file.path(path_processed_data, "stress_cleaned_data", curr_file_name))
  list_dat_all <- append(list_dat_all, list(curr_file))
}

scan_these_ids <- setdiff(x = ids_present_alternative, y = unique(c(ids_present_backup, ids_present_cloud)))
scan_these_file_names <- paste("phone_log_alternative", scan_these_ids, ".csv", sep="")

for(i in 1:length(scan_these_ids)){
  curr_file_name <- scan_these_file_names[i]
  curr_file <- read.csv(file.path(path_processed_data, "stress_cleaned_data", curr_file_name))
  list_dat_all <- append(list_dat_all, list(curr_file))
}

# -----------------------------------------------------------------------------
# Check: How many columns does each participant have in their data frame?
# A varying number of columns may influence how we perform further data parsing
# -----------------------------------------------------------------------------

count_cols <- lapply(list_dat_all, ncol)
count_cols <- unlist(count_cols)
table(count_cols)

# count_cols shows us that participants can either have 9, 10, or 11 columns
# in their parsed log file
list_dat_nine <- keep(list_dat_all, function(x){ ncol(x) == 9})
list_dat_ten <- keep(list_dat_all, function(x){ ncol(x) == 10})
list_dat_eleven <- keep(list_dat_all, function(x){ ncol(x) == 11})

# -----------------------------------------------------------------------------
# From here, we parse out information about the micro-randomizations
# e.g., intervention assignment, randomization probabilities, 
# when the micro-randomizations occurred, and parameters utilized to determine
# the randomization probabilities.
#
# These information are contained in the column emiInfo. None of the 
# participants who have nine columns in their data frame do not contain this 
# column. On the other hand, participants who have 10 or 11 columns in their
# data frame generally contain this column (with some exceptions).
#
# The actual value of the parameters used to determine the randomization
# probabilities are set here:
#
# https://github.com/MD2Korg/mCerebrum-EMAScheduler/blob/master/ema_scheduler/src/main/java/org/md2k/ema_scheduler/scheduler/emi/ProbabilityEMI.java
# -----------------------------------------------------------------------------

list_dat_rand <- list()

this_big_list <- list_dat_ten

for(i in 1:length(this_big_list)){
  curr_dat <- this_big_list[[i]]
  
  if("emiInfo" %in% colnames(curr_dat)){
    curr_dat <- curr_dat %>% 
      filter(emiInfo != "") %>% 
      select(participant_id, emiInfo)
    
    parsed_emi_info <- strsplit(curr_dat[["emiInfo"]], split = "\\{")
    parsed_emi_info <- unlist(parsed_emi_info)
    parsed_emi_info <- parsed_emi_info[parsed_emi_info != ""]
    parsed_emi_info <- strsplit(parsed_emi_info, split = "\\}")
    parsed_emi_info <- unlist(parsed_emi_info)
    parsed_emi_info <- strsplit(parsed_emi_info, ", ")
    
    for(j in 1:length(parsed_emi_info)){
      curr_array <- parsed_emi_info[[j]]
      
      idx <- startsWith(curr_array, prefix = "'G'")
      G <- substring(text = curr_array[idx], first = 6)
      G <- as.numeric(G)
      
      idx <- startsWith(curr_array, prefix = "'N'")
      N <- substring(text = curr_array[idx], first = 6)
      N <- as.numeric(N)
      
      idx <- startsWith(curr_array, prefix = "'isPreLapse'")
      isPreLapse <- substring(text = curr_array[idx], first = 15)
      isPreLapse <- ifelse(isPreLapse=="True", 1, 0)
      
      idx <- startsWith(curr_array, prefix = "'isStress'")
      isStress <- substring(text = curr_array[idx], first = 13)
      isStress <- ifelse(isStress=="True", 1, 0)
      
      idx <- startsWith(curr_array, prefix = "'isTriggered'")
      isTriggered <- substring(text = curr_array[idx], first = 16)
      isTriggered <- ifelse(isTriggered=="True", 1, 0)
      
      idx <- startsWith(curr_array, prefix = "'probability'")
      probability <- substring(text = curr_array[idx], first = 16)
      probability <- as.numeric(probability)
      
      idx <- startsWith(curr_array, prefix = "'random'")
      random <- substring(text = curr_array[idx], first = 11)
      random <- as.numeric(random)
      
      idx <- startsWith(curr_array, prefix = "'remainingTimeInMinute'")
      remainingTimeInMinute <- substring(text = curr_array[idx], first = 26)
      remainingTimeInMinute <- as.numeric(remainingTimeInMinute)
      
      idx <- startsWith(curr_array, prefix = "'sumLambda'")
      sumLambda <- substring(text = curr_array[idx], first = 14)
      sumLambda <- as.numeric(sumLambda)
      
      idx <- startsWith(curr_array, prefix = "'timestamp'")
      timestamp <- substring(text = curr_array[idx], first = 14)
      timestamp <- as.numeric(timestamp)
      
      parsed_emi_info[[j]] <- c(G=G, N=N, isPreLapse=isPreLapse,
                                isStress=isStress, isTriggered=isTriggered,
                                probability=probability, random=random,
                                remainingTimeInMinute=remainingTimeInMinute,
                                sumLambda=sumLambda, timestamp=timestamp)
    }
    
    parsed_emi_info <- bind_rows(parsed_emi_info)
    curr_dat <- bind_cols(curr_dat, parsed_emi_info)
    curr_dat <- curr_dat %>% mutate(time_unixts = timestamp/1000) %>%
      mutate(time_hrts_utc = as.POSIXct(x = time_unixts, tz = "UTC", origin = "1970-01-01")) %>%
      mutate(time_hrts_local = with_tz(time = time_hrts_utc, tzone = "America/Chicago")) %>%
      select(participant_id, emiInfo, 
             time_hrts_utc, time_hrts_local, time_unixts, 
             all_of(names(parsed_emi_info)))
    
    list_dat_rand <- append(list_dat_rand, list(curr_dat))
  }else{
    next
  }
}

this_big_list <- list_dat_eleven

for(i in 1:length(this_big_list)){
  curr_dat <- this_big_list[[i]]
  
  if("emiInfo" %in% colnames(curr_dat)){
    curr_dat <- curr_dat %>% 
      filter(emiInfo != "") %>% 
      select(participant_id, emiInfo)
    
    parsed_emi_info <- strsplit(curr_dat[["emiInfo"]], split = "\\{")
    parsed_emi_info <- unlist(parsed_emi_info)
    parsed_emi_info <- parsed_emi_info[parsed_emi_info != ""]
    parsed_emi_info <- strsplit(parsed_emi_info, split = "\\}")
    parsed_emi_info <- unlist(parsed_emi_info)
    parsed_emi_info <- strsplit(parsed_emi_info, ", ")
    
    for(j in 1:length(parsed_emi_info)){
      curr_array <- parsed_emi_info[[j]]
      
      idx <- startsWith(curr_array, prefix = "'G'")
      G <- substring(text = curr_array[idx], first = 6)
      G <- as.numeric(G)
      
      idx <- startsWith(curr_array, prefix = "'N'")
      N <- substring(text = curr_array[idx], first = 6)
      N <- as.numeric(N)
      
      idx <- startsWith(curr_array, prefix = "'isPreLapse'")
      isPreLapse <- substring(text = curr_array[idx], first = 15)
      isPreLapse <- ifelse(isPreLapse=="True", 1, 0)
      
      idx <- startsWith(curr_array, prefix = "'isStress'")
      isStress <- substring(text = curr_array[idx], first = 13)
      isStress <- ifelse(isStress=="True", 1, 0)
      
      idx <- startsWith(curr_array, prefix = "'isTriggered'")
      isTriggered <- substring(text = curr_array[idx], first = 16)
      isTriggered <- ifelse(isTriggered=="True", 1, 0)
      
      idx <- startsWith(curr_array, prefix = "'probability'")
      probability <- substring(text = curr_array[idx], first = 16)
      probability <- as.numeric(probability)
      
      idx <- startsWith(curr_array, prefix = "'random'")
      random <- substring(text = curr_array[idx], first = 11)
      random <- as.numeric(random)
      
      idx <- startsWith(curr_array, prefix = "'remainingTimeInMinute'")
      remainingTimeInMinute <- substring(text = curr_array[idx], first = 26)
      remainingTimeInMinute <- as.numeric(remainingTimeInMinute)
      
      idx <- startsWith(curr_array, prefix = "'sumLambda'")
      sumLambda <- substring(text = curr_array[idx], first = 14)
      sumLambda <- as.numeric(sumLambda)
      
      idx <- startsWith(curr_array, prefix = "'timestamp'")
      timestamp <- substring(text = curr_array[idx], first = 14)
      timestamp <- as.numeric(timestamp)
      
      parsed_emi_info[[j]] <- c(G=G, N=N, isPreLapse=isPreLapse,
                                isStress=isStress, isTriggered=isTriggered,
                                probability=probability, random=random,
                                remainingTimeInMinute=remainingTimeInMinute,
                                sumLambda=sumLambda, timestamp=timestamp)
    }
    
    parsed_emi_info <- bind_rows(parsed_emi_info)
    curr_dat <- bind_cols(curr_dat, parsed_emi_info)
    curr_dat <- curr_dat %>% mutate(time_unixts = timestamp/1000) %>%
      mutate(time_hrts_utc = as.POSIXct(x = time_unixts, tz = "UTC", origin = "1970-01-01")) %>%
      mutate(time_hrts_local = with_tz(time = time_hrts_utc, tzone = "America/Chicago")) %>%
      select(participant_id, emiInfo, 
             time_hrts_utc, time_hrts_local, time_unixts, 
             all_of(names(parsed_emi_info)))
    
    list_dat_rand <- append(list_dat_rand, list(curr_dat))
  }else{
    next
  }
}


dat_rand <- bind_rows(list_dat_rand)
dat_rand <- dat_rand %>% 
  select(-timestamp) %>%
  arrange(participant_id, time_unixts)

# To enable subsequent merging by participant ID and calendar date
dat_rand <- dat_rand %>% 
  mutate(date_utc = date(time_hrts_utc),
         date_local = date(time_hrts_local)) 

dat_rand <- dat_rand %>%
  mutate(ones = 1) %>%
  mutate(emi_id = cumsum(ones)) %>%
  select(-ones)

# -----------------------------------------------------------------------------
# Prepare to save parsed data to an RData file in preparation for
# merging with other data sources
# -----------------------------------------------------------------------------

dat_rand <- rename_with(dat_rand, ~paste("rand_", .x, sep=""), starts_with("time_"))
parsed_dat_rand <- dat_rand

save(parsed_dat_rand, file = file.path(path_staged_data, "parsed_dat_rand.RData"))

