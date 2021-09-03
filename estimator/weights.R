library(Matrix)
library(parallel)
library(rootSolve)
path_staged_data <- Sys.getenv("path_staged_data")
load(file.path(path_staged_data, "list_allroots_ee_rho.RData"))
load(file.path(path_staged_data, "mcar.RData"))

