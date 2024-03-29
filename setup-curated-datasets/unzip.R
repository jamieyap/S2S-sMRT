library(dplyr)
library(lubridate)
library(purrr)
source("paths.R")

# -----------------------------------------------------------------------------
# Unzip phone backup raw data files
# -----------------------------------------------------------------------------

current_path_to_inspect <- path_raw_data_phone_backup
all_file_names <- list.files(current_path_to_inspect)
relevant_file_names <- all_file_names[grepl(".zip", all_file_names)]

for(i in 1:length(relevant_file_names)){
  this_file <- relevant_file_names[i]
  this_participant_id <- substr(x = this_file, start = 1, stop = 3)

  if(dir.exists(file.path(path_raw_data_unzipped, "backup", this_participant_id)) == FALSE){
    dir.create(file.path(path_raw_data_unzipped, "backup", this_participant_id))
  }

  df_allfiles <- unzip(zipfile = file.path(current_path_to_inspect, this_file), list = TRUE)
  arr_files <- c(df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null\\_DATA.csv", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null.json", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_RIP\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null\\_DATA.csv", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_RIP\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null.json", x = df_allfiles$Name)])

  if(substr(x = df_allfiles$Name[1], start = 1, stop = 3) == this_participant_id){
    unzip(zipfile = file.path(current_path_to_inspect, this_file),
          files = arr_files,
          exdir =  file.path(path_raw_data_unzipped, "backup", this_participant_id),
          overwrite = TRUE,
          junkpaths = TRUE)
  }else{
    unzip(zipfile = file.path(current_path_to_inspect, this_file),
          files = arr_files,
          exdir =  file.path(path_raw_data_unzipped, "backup", this_participant_id),
          overwrite = TRUE,
          junkpaths = TRUE)
  }
}


# -----------------------------------------------------------------------------
# Unzip cloud raw data files
# -----------------------------------------------------------------------------

current_path_to_inspect <- path_raw_data_phone_cloud
all_file_names <- list.files(current_path_to_inspect)
relevant_file_names <- all_file_names[grepl(".zip", all_file_names)]
relevant_file_names <- relevant_file_names[!grepl("Selected_Data_Streams", relevant_file_names)]

for(i in 1:length(relevant_file_names)){
  this_file <- relevant_file_names[i]
  this_participant_id <- substr(x = this_file, start = 1, stop = 3)

  if(dir.exists(file.path(path_raw_data_unzipped, "cloud", this_participant_id)) == FALSE){
    dir.create(file.path(path_raw_data_unzipped, "cloud", this_participant_id))
  }

  df_allfiles <- unzip(zipfile = file.path(current_path_to_inspect, this_file), list = TRUE)
  arr_files <- c(df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\+PHONE.csv.bz2", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\+PHONE.json", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_RIP\\+PHONE.csv.bz2", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_RIP\\+PHONE.json", x = df_allfiles$Name)])

  unzip(zipfile = file.path(current_path_to_inspect, this_file),
        files = arr_files,
        exdir =  file.path(path_raw_data_unzipped, "cloud", this_participant_id),
        overwrite = TRUE,
        junkpaths = TRUE)
}

# -----------------------------------------------------------------------------
# Unzip alternative source raw data files
# -----------------------------------------------------------------------------

current_path_to_inspect <- path_raw_data_phone_alternative
all_file_names <- list.files(current_path_to_inspect)
relevant_file_names <- all_file_names[grepl("export.zip", all_file_names)]

for(i in 1:length(relevant_file_names)){
  this_file <- relevant_file_names[i]
  this_participant_id <- substr(x = this_file, start = 1, stop = 3)
  
  if(dir.exists(file.path(path_raw_data_unzipped, "alternative", this_participant_id)) == FALSE){
    dir.create(file.path(path_raw_data_unzipped, "alternative", this_participant_id))
  }
  
  df_allfiles <- unzip(zipfile = file.path(current_path_to_inspect, this_file), list = TRUE)
  arr_files <- c(df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null\\_DATA.csv", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null.json", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_RIP\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null\\_DATA.csv", x = df_allfiles$Name)],
                 df_allfiles$Name[grepl(pattern = "CSTRESS\\_FEATURE\\_VECTOR\\_RIP\\_null\\_PHONE\\_null\\_null\\_org.md2k.streamprocessor\\_null.json", x = df_allfiles$Name)])
  
  unzip(zipfile = file.path(current_path_to_inspect, this_file), 
        files = arr_files,
        exdir =  file.path(path_raw_data_unzipped, "alternative", this_participant_id),
        overwrite = TRUE,
        junkpaths = TRUE)
}

