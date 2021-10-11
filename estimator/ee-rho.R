library(Matrix)
library(parallel)
library(rootSolve)
source("paths.R")
load(file.path(path_staged_data, "mcar.RData"))

# -----------------------------------------------------------------------------
# Define estimating equation
# -----------------------------------------------------------------------------

ModelEERho <- function(dat, values){
  
  RHO0 <- values[1]
  RHO1 <- values[2]
  
  delta <- ifelse(dat[,"Xobsk"]==0, dat[,"probAk"] - RHO0, dat[,"probAk"] - RHO1)
  delta <- ifelse(dat[,"Ik"]==0, NA_real_, delta)
  dat <- cbind(dat, delta)
  
  N_participants <- length(unique(dat[,"id"]))
  total_decision_points <- length(unique(dat[,"dp"]))
  
  arr_stressed <- c()
  arr_not_stressed <- c()
  
  for(idx_person in 1:N_participants){
    
    # Select rows corresponding to one particular person
    dat_current_person <- dat[which(dat[,"id"] == idx_person),]
    
    # For this particular person, select time-points for which they are available
    # and further, among available time-points, select only those time-points
    # for which we have observed data (i.e., not missing)
    select_these_rows <- ((dat_current_person[,"Ik"]==1) & (!is.na(dat_current_person[,"Xobsk"])))
    dat_current_person <- dat_current_person[which(select_these_rows),]
    
    # Afterwards, calculate delta corresponding to decision-points
    # classified as stressed and not stressed, respectively
    dat_stressed <- dat_current_person[which(dat_current_person[,"Xobsk"]==1),]
    dat_not_stressed <- dat_current_person[which(dat_current_person[,"Xobsk"]==0),]
    
    # This is the sum of delta across this participant's decision-points
    # The next two lines assume that all participants in the dataset
    # have at least one observed decision-point classified as stressed
    # and at least one observed decision-point classified as not stressed
    tot_delta_stressed <- sum(dat_stressed[,"delta"])
    tot_delta_not_stressed <- sum(dat_not_stressed[,"delta"])
    
    # Append results to array; each element of the array
    # corresponds to a particular participant's total value of delta
    # across their decision-points
    arr_stressed <- c(arr_stressed, tot_delta_stressed)
    arr_not_stressed <- c(arr_not_stressed, tot_delta_not_stressed)
  }
  
  # Now, we return the mean of the elements of arr_stressed across all
  # participants; we also return the mean of the elements of arr_not_stressed
  # across all participants
  
  return(c(mean(arr_stressed), mean(arr_not_stressed)))
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
                                    
                                    allroots_ee <- multiroot(f = ModelEERho, start = c(0,0), dat = current_simdat)
                                    simnum <- as.numeric(current_simdat[1,"simnum"])
                                    # 1st element of estimates would be \hat{RHO0}
                                    # 2nd element of estimates would be \hat{RHO1}
                                    estimates <- allroots_ee[["root"]]
                                    pardat <- c(simnum, estimates)
                                    
                                    return(pardat)
                                  })

stopCluster(cl)

allroots_ee_rho <- do.call(rbind, list_allroots_ee_rho)
dimnames(allroots_ee_rho) <- list(NULL, c("simnum", "RHO0", "RHO1"))
allroots_ee_rho <- Matrix::Matrix(allroots_ee_rho)

save(allroots_ee_rho, file = file.path(path_staged_data, "allroots_ee_rho.RData"))

