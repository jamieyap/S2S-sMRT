.start_ts1 <- Sys.time()
source("data-generating-models/generate-mcar.R")
rm(list = ls())

.start_ts2 <- Sys.time()
source("estimator/ee-rho.R")
rm(list = ls())

.start_ts3 <- Sys.time()
source("estimator/weights.R")
rm(list = ls())

.start_ts4 <- Sys.time()
source("estimator/ee-missingness-mechanism.R")
rm(list = ls())

.start_ts5 <- Sys.time()
source("estimator/ee-causal-effect.R")
rm(list = ls())

.start_ts6 <- Sys.time()

source("paths.R")
save(.start_ts1,
     .start_ts2,
     .start_ts3,
     .start_ts4,
     .start_ts5,
     .start_ts6,
     file = file.path(path_staged_data, "runtime.RData"))

