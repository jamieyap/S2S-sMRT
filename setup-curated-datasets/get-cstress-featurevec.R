library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

# -----------------------------------------------------------------------------
# First, check phone backup raw data files
# -----------------------------------------------------------------------------

current_path_to_inspect <- file.path(path_raw_data_unzipped, "backup")
all_file_names <- list.files(current_path_to_inspect)
collect_participant_id <- list.files(current_path_to_inspect)
list_data <- list()

for(i in 1:length(all_file_names)){
  this_participant_id <- all_file_names[i]
  all_files <- list.files(file.path(current_path_to_inspect, this_participant_id))
  # This file is where data on Heart Rate is located
  this_file_name <- all_files[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null\\_DATA.csv", x = all_files)]
  
  if(length(this_file_name) == 1){
    grabbed_file <- read.csv(file.path(current_path_to_inspect, this_participant_id, this_file_name), header = FALSE)
    num_obs <- nrow(grabbed_file)
    current_dat <- data.frame(participant_id = rep(this_participant_id, num_obs),
                              cstress_featurevec_unixts = grabbed_file[,1],
                              is_exist = rep(1, num_obs))
    list_data <- append(list_data, list(current_dat))
  }else if(length(this_file_name) > 1){
    print("Duplicates!")  # No duplicates observed after running this loop
  }else{
    next
  }
}

# -----------------------------------------------------------------------------
# Second, check cloud raw data files
# -----------------------------------------------------------------------------

current_path_to_inspect <- file.path(path_raw_data_unzipped, "backup")
observed_participant_id <- list.files(current_path_to_inspect)
check_overlap <- setdiff(observed_participant_id, collect_participant_id)

if(length(check_overlap)==0){
  print("No need to check cloud")  # TRUE
}else{
  print("Data exists in cloud that is not present in backup")
}

# -----------------------------------------------------------------------------
# Third, check alternative raw data files
# -----------------------------------------------------------------------------

current_path_to_inspect <- file.path(path_raw_data_unzipped, "alternative")
observed_participant_id <- list.files(current_path_to_inspect)
check_overlap <- setdiff(observed_participant_id, collect_participant_id)

if(length(check_overlap)==0){
  print("No need to check alternative")  # FALSE
}else{
  print("Data exists in alternative that is not present in backup")
}

all_file_names <- check_overlap
collect_participant_id <- c(collect_participant_id, check_overlap)

for(i in 1:length(all_file_names)){
  this_participant_id <- all_file_names[i]
  all_files <- list.files(file.path(current_path_to_inspect, this_participant_id))
  # This file is where data on Heart Rate is located
  this_file_name <- all_files[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null\\_DATA.csv", x = all_files)]
  
  if(length(this_file_name) == 1){
    grabbed_file <- read.csv(file.path(current_path_to_inspect, this_participant_id, this_file_name), header = FALSE)
    num_obs <- nrow(grabbed_file)
    current_dat <- data.frame(participant_id = rep(this_participant_id, num_obs),
                              cstress_featurevec_unixts = grabbed_file[,1],
                              is_exist = rep(1, num_obs))
    list_data <- append(list_data, list(current_dat))
  }else if(length(this_file_name) > 1){
    print("Duplicates!")  # No duplicates observed after running this loop
  }else{
    next
  }
}

# -----------------------------------------------------------------------------
# Create additional time variables
# -----------------------------------------------------------------------------

cstress_featurevec <- do.call(rbind, list_data)
cstress_featurevec <- cstress_featurevec %>%
  mutate(cstress_featurevec_unixts = cstress_featurevec_unixts/1000) %>%
  mutate(cstress_featurevec_hrts_utc = as.POSIXct(x = cstress_featurevec_unixts, tz = "UTC", origin = "1970-01-01")) %>%
  mutate(cstress_featurevec_hrts_local = with_tz(time = cstress_featurevec_hrts_utc, tzone = "America/Chicago")) %>%
  select(participant_id, cstress_featurevec_unixts, cstress_featurevec_hrts_utc, cstress_featurevec_hrts_local, everything())

# -----------------------------------------------------------------------------
# Save output as RData file
# -----------------------------------------------------------------------------

save(cstress_featurevec, file = file.path(path_staged_data, "cstress_featurevec.RData"))

