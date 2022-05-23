# -----------------------------------------------------------------------------
# Raw data extraction
# -----------------------------------------------------------------------------

# Set condition to TRUE when running this script for the first time
if(TRUE){
  # This is a very time-intensive operation
  source("setup-curated-datasets/unzip.R")
  rm(list = ls())
}

# Extract timestamps from cStress feature vector that pertain to when
# Heart Rate was calculated
source("setup-curated-datasets/get-cstress-featurevec.R")
rm(list = ls())

# Extract start-of-day button press data
source("setup-curated-datasets/get-start-day-button-press-data.R")
rm(list = ls())

# Extract predictions of activity detection algorithm
source("setup-curated-datasets/get-activity-data.R")
rm(list = ls())

# Extract raw data on micro-randomization
source("setup-curated-datasets/get-randomization-data.R")
rm(list = ls())

# Extract raw data on stress episode classification
source("setup-curated-datasets/get-stress-episode-data.R")
rm(list = ls())

# -----------------------------------------------------------------------------
# Create a data frame containing a list of ID's of all participants
# who were enrolled into the Sense2Stop study, and then determine whether
# they meet any of the criteria for them to not be included in any analyses
# -----------------------------------------------------------------------------

source("setup-curated-datasets/create-masterlist.R")
rm(list = ls())

# Note that the next script requires the output of 
# get-randomization-data.R and get-stress-episode-data.R
source("setup-curated-datasets/create-masterlist-continued.R")
rm(list = ls())

# -----------------------------------------------------------------------------
# Create two data frames in long format 
#   (i) each row pertains to a participant-day
#   (ii) each row pertains to a participant-day-minute
# -----------------------------------------------------------------------------

# Note that the "skeleton" produced will not include those participants excluded
#  from all data analysis, i.e., it will only include the remaining 49 participants
source("setup-curated-datasets/create-skeleton.R")
rm(list = ls())

# -----------------------------------------------------------------------------
# Create a data frame in long format with the Yit's
# -----------------------------------------------------------------------------

# Note that this script is where episodes (all types) for which peak
# is not within first day of mrt and last day of mrt are removed prior to
# all subsequent data processing steps
# Note that the output produced by this script will not include those participants excluded
# from all data analysis, i.e., it will only include the remaining 49 participants
source("setup-curated-datasets/construct-episodes-physical-activity.R")
rm(list = ls())

# Set condition to TRUE when running this script for the first time
if(TRUE){
  source("check-intermediate-datasets/check-episode-length.R")
  rm(list = ls())
}

source("setup-curated-datasets/construct-heart-rate-indicators.R")
rm(list = ls())

# Set condition to TRUE when running this script for the first time
if(TRUE){
  source("check-intermediate-datasets/check-existence-heart-rate-data.R")
  rm(list = ls())
}

source("setup-curated-datasets/censor-episodes.R")
rm(list = ls())

# Set condition to TRUE when running this script for the first time
if(TRUE){
  source("check-intermediate-datasets/check-after-censoring.R")
  rm(list = ls())
}

source("setup-curated-datasets/censor-more-episodes.R")
rm(list = ls())

# Set condition to TRUE when running this script for the first time
if(TRUE){
  source("check-intermediate-datasets/check-after-more-censoring.R")
  rm(list = ls())
}

source("setup-curated-datasets/construct-minute-by-minute-classification.R")
rm(list = ls())

# -----------------------------------------------------------------------------
# Merge randomization assignment data with minute-by-minute episode
# classification data and identify those micro-randomizations which
# will be used to estimate the treatment effect
# -----------------------------------------------------------------------------

source("setup-curated-datasets/construct-stratification-variable.R")
rm(list = ls())

# Set condition to TRUE when running this script for the first time
if(TRUE){
  source("check-intermediate-datasets/check-availability.R")
  rm(list = ls())
}

# Set condition to TRUE when running this script for the first time
if(TRUE){
  source("check-intermediate-datasets/check-stratification-variable.R")
  rm(list = ls())
}

# -----------------------------------------------------------------------------
# Link randomization assignment to the minute-by-minute classification
# -----------------------------------------------------------------------------

source("setup-curated-datasets/link.R")
rm(list = ls())

