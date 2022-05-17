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

dat_stress <- dat_rand_for_treatment_effect_estimation %>% filter(isStress==1)
dat_not_stress <- dat_rand_for_treatment_effect_estimation %>% filter(isStress==0)

tab <- tab %>% arrange(desc(isStress))
tab[["total"]] <- c(nrow(dat_stress), nrow(dat_not_stress))
tab[["mean"]] <- c(mean(dat_stress[["probability"]]), mean(dat_not_stress[["probability"]]))
tab[["sd"]] <- c(sd(dat_stress[["probability"]]), sd(dat_not_stress[["probability"]]))

write.csv(tab, file.path("check-intermediate-datasets", "collect-output", "rand_probs_summary.csv"), row.names = FALSE)

