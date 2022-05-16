library(dplyr)
library(lubridate)
library(purrr)
library(parallel)
source("paths.R")

# -----------------------------------------------------------------------------
# Load relevant datasets
# -----------------------------------------------------------------------------
load(file.path(path_staged_data, "dat_rand_for_treatment_effect_estimation.RData"))

# -----------------------------------------------------------------------------
# Check distribution of randomization probabilities
# -----------------------------------------------------------------------------

tab <- dat_rand_for_treatment_effect_estimation %>%
  group_by(isStress) %>%
  summarise(num_exactly_zero = sum(probability == 0),
            num_exactly_one = sum(probability == 1))

print(tab)
