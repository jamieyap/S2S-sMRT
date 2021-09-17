library(Matrix)
library(parallel)
library(rootSolve)
source("paths.R")
source("params.R")
load(file.path(path_staged_data, "allroots_ee_rho.RData"))
load(file.path(path_staged_data, "weightedsimlist.RData"))

# -----------------------------------------------------------------------------
# Commentary on params required from params.R
# -----------------------------------------------------------------------------

# tot_decision_points
# tot_excursion_length

# -----------------------------------------------------------------------------
# Data preparation steps:
# Center the intervention assignment at each decision point k
# -----------------------------------------------------------------------------

for(idx_sim in 1:N_sim){
  
  dat <- weightedsimlist[[idx_sim]]
  N_participants <- length(unique(dat[,"id"]))
  RHO0 <- allroots_ee_rho[idx_sim,"RHO0"]
  RHO1 <- allroots_ee_rho[idx_sim,"RHO1"]
  
  cAk <- ifelse(dat[,"Xk"]==0, 
                dat[,"Ak"] - RHO0,
                dat[,"Ak"] - RHO1)
  
  dat <- cbind(dat, cAk = cAk)
  weightedsimlist[[idx_sim]] <- dat
}

# -----------------------------------------------------------------------------
# Define estimating equation
# -----------------------------------------------------------------------------

ModelEEObservedDataIndicator <- function(dat, 
                                         values,
                                         tot_decision_points,
                                         tot_excursion_length){
  
  N_participants <- length(unique(dat[,"id"]))
  PSI0 <- values[1]
  ETA0 <- values[2]
  
  running_total_upper_block <- 0
  running_total_lower_block <- 0
  
  for(idx_person in 1:N_participants){
    these_rows <- (dat[,"id"] == idx_person)
    dat_person <- dat[these_rows,]
    
    # Step 1: Calculate O_{t+m} for all m = 1, 2, 3, ..., tot_excursion_length
    columns_copied <- rep(list(dat_person[,"Ok"]), tot_excursion_length)
    columns_rolled <- mapply(FUN = tail, 
                             x = columns_copied, 
                             n = -c(1:tot_excursion_length), 
                             SIMPLIFY = FALSE)
    columns_rolled <- mapply(FUN = function(v,m){c(v, rep(NA,m))}, 
                             v = columns_rolled, 
                             m = c(1:tot_excursion_length), 
                             SIMPLIFY = TRUE)
    dimnames(columns_rolled) <- list(NULL, paste("Ok", "plus", 1:tot_excursion_length, sep="_"))
    
    # Step 2: Work with upper block
    mat_excursion_upper_block <- apply(X = columns_rolled,
                                       MARGIN = 2,
                                       FUN = function(curr_col, 
                                                      psi = PSI0,
                                                      eta = ETA0,
                                                      thisdat = dat_person){
                                         val <- thisdat[,"Ik"] * thisdat[,"Wk"] * (exp(-thisdat[,"Ak"] * eta) * curr_col - exp(-psi))
                                         return(val)
                                       })
    
    these_rows <- (dat_person[,"Ik"]==0)
    mat_excursion_upper_block[these_rows,] <- rep(0, tot_excursion_length)
    Sk_upper_block <- rowSums(mat_excursion_upper_block[1:(tot_decision_points - tot_excursion_length),])
    upper_block <- sum(Sk_upper_block)
    running_total_upper_block <- running_total_upper_block + upper_block
    
    # Step 3: Work with lower block
    mat_excursion_lower_block <- apply(X = columns_rolled,
                                       MARGIN = 2,
                                       FUN = function(curr_col,
                                                      psi = PSI0,
                                                      eta = ETA0,
                                                      thisdat = dat_person){
                                         val <- thisdat[,"Ik"] * thisdat[,"Wk"] * (exp(-thisdat[,"Ak"] * eta) * curr_col - exp(-psi)) * thisdat[,"cAk"]
                                         return(val)
                                       })
    
    mat_excursion_lower_block[these_rows,] <- rep(0, tot_excursion_length)
    Sk_lower_block <- rowSums(mat_excursion_lower_block[1:(tot_decision_points - tot_excursion_length),])
    lower_block <- sum(Sk_lower_block)
    running_total_lower_block <- running_total_lower_block + lower_block
  }
  
  return(c(running_total_upper_block/N_participants,
           running_total_lower_block/N_participants))
}

# -----------------------------------------------------------------------------
# Get roots using each simulated dataset
# -----------------------------------------------------------------------------

cl <- makeCluster(getOption("cl.cores", detectCores()-1))
clusterSetRNGStream(cl = cl, iseed = 174125)
clusterExport(cl, c("ModelEEObservedDataIndicator",
                    "weightedsimlist",
                    "tot_decision_points",
                    "tot_excursion_length"))
clusterEvalQ(cl, {
  library(Matrix)
  library(rootSolve)
})

list_allroots_ee_missdat <- parLapply(cl = cl, 
                                      X = weightedsimlist,
                                      fun = function(current_simdat){
                                    
                                        allroots_ee <- multiroot(f = ModelEEObservedDataIndicator, 
                                                                 start = c(0.1, 0.1), 
                                                                 dat = current_simdat,
                                                                 tot_decision_points = tot_decision_points,
                                                                 tot_excursion_length = tot_excursion_length)
                                        
                                        pardat <- c(as.numeric(current_simdat[1,"simnum"]), 
                                                    allroots_ee[["root"]])
                                        
                                        return(pardat)
                                        })

stopCluster(cl)

allroots_ee_missdat <- do.call(rbind, list_allroots_ee_missdat)
dimnames(allroots_ee_missdat) <- list(NULL, c("simnum", "ETA0", "PSI0"))
allroots_ee_missdat <- Matrix::Matrix(allroots_ee_missdat)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------
save(allroots_ee_missdat,
     file = file.path(path_staged_data, "allroots_ee_missdat.RData"))
