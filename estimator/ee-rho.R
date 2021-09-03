library(Matrix)
library(parallel)
library(rootSolve)
path_staged_data <- Sys.getenv("path_staged_data")
load(file.path(path_staged_data, "mcar.RData"))

# -----------------------------------------------------------------------------
# Define estimating equation
# -----------------------------------------------------------------------------

ModelEERho <- function(values, dat){
  rho0 <- values[1]
  rho1 <- values[2]
  delta <- ifelse(dat[,"Xk"]==0, dat[,"probAk"] - rho0, dat[,"probAk"] - rho1)
  dat <- cbind(dat, delta)
  
  N_participants <- length(unique(dat[,"id"]))
  total_decision_points <- length(unique(dat[,"dp"]))
  all_people_total_0 <- 0
  all_people_total_1 <- 1
  for(idx_person in 1:N_participants){
    select_these_rows <- dat[,"Xk"]==0
    this_person_total_0 <- sum(dat[select_these_rows,"delta"], na.rm=TRUE)
    all_people_total_0 <- all_people_total_0 + this_person_total_0
    
    select_these_rows <- dat[,"Xk"]==1
    this_person_total_1 <- sum(dat[select_these_rows,"delta"], na.rm=TRUE)
    all_people_total_1 <- all_people_total_1 + this_person_total_1
  }
  
  return(c(all_people_total_0/N_participants,
           all_people_total_1/N_participants))
}

# -----------------------------------------------------------------------------
# Get roots using each simulated dataset
# -----------------------------------------------------------------------------

N_sim <- length(simlist)
list_allroots_ee_rho <- list()

for(idx_sim in 1:N_sim){
  dat <- simlist[[idx_sim]]
  allroots_ee_rho <- multiroot(f = ModelEERho, start = c(0.1, 0.1), dat = dat)
  list_allroots_ee_rho <- append(list_allroots_ee_rho, list(allroots_ee_rho))
}

save(list_allroots_ee_rho, file = file.path(path_staged_data, "list_allroots_ee_rho.RData"))

