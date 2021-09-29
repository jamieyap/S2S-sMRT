library(Matrix)
library(parallel)
library(rootSolve)
source("paths.R")
load(file.path(path_simulated_data, "mcar.RData"))

# -----------------------------------------------------------------------------
# Define estimating equation
# -----------------------------------------------------------------------------

ModelEERho <- function(dat, values){
  RHO0 <- values[1]
  RHO1 <- values[2]
  delta <- ifelse(dat[,"Xk"]==0, dat[,"probAk"] - RHO0, dat[,"probAk"] - RHO1)
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

cl <- makeCluster(getOption("cl.cores", detectCores()-1))
clusterExport(cl, c("ModelEERho","simlist"))
clusterSetRNGStream(cl = cl, iseed = 174125)
clusterEvalQ(cl, {
  library(Matrix)
  library(rootSolve)
})

list_allroots_ee_rho <- parLapply(cl = cl, 
                                  X = simlist,
                                  fun = function(current_simdat){
                                    
                                    allroots_ee <- multiroot(f = ModelEERho, 
                                                             start = c(0.1, 0.1), 
                                                             dat = current_simdat)
                                    
                                    pardat <- c(as.numeric(current_simdat[1,"simnum"]), 
                                                allroots_ee[["root"]])
                                    
                                    return(pardat)
                                  })

stopCluster(cl)

allroots_ee_rho <- do.call(rbind, list_allroots_ee_rho)
dimnames(allroots_ee_rho) <- list(NULL, c("simnum", "RHO0", "RHO1"))
allroots_ee_rho <- Matrix::Matrix(allroots_ee_rho)

save(allroots_ee_rho, file = file.path(path_simulated_data, "allroots_ee_rho.RData"))

