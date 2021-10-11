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
  
  # Note: If Ik==1, it is possible for Xk to be a missing value
  # Note: If Xobsk is missing (i.e., has a value of NA)
  # then cAk will take on a missing value (i.e., NA) as well
  cAk <- ifelse(dat[,"Xobsk"]==0, 
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
    # Take subset of rows corresponding to one particular participant
    these_rows <- (dat[,"id"] == idx_person)
    dat_person <- dat[these_rows,]
    
    # Step 1: Calculate O_{k+m} for all m = 1, 2, 3, ..., tot_excursion_length
    # First, we create tot_excursion_length number of copes of the column Ok
    # We will use each copy as a starting point to calculate O_{k+m}
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
    # Note: Currently, mat_excursion_upper_block is a TxM-dimensional matrix
    mat_excursion_upper_block <- apply(X = columns_rolled,
                                       MARGIN = 2,
                                       FUN = function(curr_col, 
                                                      psi = PSI0,
                                                      eta = ETA0,
                                                      thisdat = dat_person){
                                         
                                         # Note what is held constant in the calculation of val and what is varied
                                         # Ik, Wk, Ak, cAk are held constant
                                         # curr_col is varied
                                         val <- thisdat[,"Ik"] * thisdat[,"Wk"] * (exp(-thisdat[,"Ak"] * eta) * curr_col - exp(psi))
                                         return(val)
                                       })
    
    # Note: row names will be transformed to a character value (i.e., will not remain a numeric value)
    dimnames(mat_excursion_upper_block)[[1]] <- (1:tot_decision_points)
    all_row_names <- dimnames(mat_excursion_upper_block)[[1]]
    valid_row_names <- paste(1:(tot_decision_points - tot_excursion_length))
    valid_decision_points <- (all_row_names %in% valid_row_names)
    
    # Determine which rows (i.e., which among t=1,2,3,...,T) to use in the sum for the upper block
    keep_these_rows <- (dat_person[,"Ik"]==1 & dat_person[,"Ok"]==1)
    
    # Now, mat_excursion_upper_block is a SXM-dimensional matrix, where S<=T
    mat_excursion_upper_block <- mat_excursion_upper_block[valid_decision_points & keep_these_rows,]
    
    # First, sum across m=1,2,3,...,M (these are the columns of mat_excursion_upper_block)
    Sk_upper_block <- rowSums(mat_excursion_upper_block)
    # Next, sum across t=1,2,3,...,T (some decision points t would have been excluded at this point)
    upper_block <- sum(Sk_upper_block)
    # Update the running total for the upper block
    running_total_upper_block <- running_total_upper_block + upper_block
    
    # Step 3: Work with lower block
    # Note: Currently, mat_excursion_lower_block is a TXM-dimensional matrix
    mat_excursion_lower_block <- apply(X = columns_rolled,
                                       MARGIN = 2,
                                       FUN = function(curr_col,
                                                      psi = PSI0,
                                                      eta = ETA0,
                                                      thisdat = dat_person){
                                         
                                         # Note what is held constant in the calculation of val and what is varied
                                         # Ik, Wk, Ak, cAk are held constant
                                         # curr_col is varied
                                         val <- thisdat[,"Ik"] * thisdat[,"Wk"] * (exp(-thisdat[,"Ak"] * eta) * curr_col - exp(psi)) * thisdat[,"cAk"]
                                         return(val)
                                       })
    
    # Note: row names will be transformed to a character value (i.e., will not remain a numeric value)
    dimnames(mat_excursion_lower_block)[[1]] <- (1:tot_decision_points)
    all_row_names <- dimnames(mat_excursion_lower_block)[[1]]
    valid_row_names <- paste(1:(tot_decision_points - tot_excursion_length))
    valid_decision_points <- (all_row_names %in% valid_row_names)
    
    # Determine which rows (i.e., which among t=1,2,3,...,T) to use in the sum for the lower block
    keep_these_rows <- (dat_person[,"Ik"]==1 & dat_person[,"Ok"]==1)
    
    # Now, mat_excursion_lower_block is a SXM-dimensional matrix, where S<=T
    mat_excursion_lower_block <- mat_excursion_lower_block[valid_decision_points & keep_these_rows,]
    
    # First, sum across m=1,2,3,...,M (these are the columns of mat_excursion_lower_block)
    Sk_lower_block <- rowSums(mat_excursion_lower_block)
    # Next, sum across t=1,2,3,...,T (some decision points t would have been excluded at this point)
    lower_block <- sum(Sk_lower_block)
    # Update the running total for the lower block
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
                                                                 start = c(0,0), 
                                                                 dat = current_simdat,
                                                                 tot_decision_points = tot_decision_points,
                                                                 tot_excursion_length = tot_excursion_length)
                                        
                                        simnum <- as.numeric(current_simdat[1,"simnum"])
                                        # 1st element of estimates would be \hat{PSI0}
                                        # 2nd element of estimates would be \hat{ETA0}
                                        estimates <- allroots_ee[["root"]]
                                        pardat <- c(simnum, estimates)
                                        
                                        return(pardat)
                                        })

stopCluster(cl)

allroots_ee_missdat <- do.call(rbind, list_allroots_ee_missdat)
dimnames(allroots_ee_missdat) <- list(NULL, c("simnum", "PSI0", "ETA0"))
allroots_ee_missdat <- Matrix::Matrix(allroots_ee_missdat)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

print(exp(allroots_ee_missdat[,"PSI0"]))

print(exp(allroots_ee_missdat[,"PSI0"] + allroots_ee_missdat[,"ETA0"]))

save(allroots_ee_missdat,
     file = file.path(path_staged_data, "allroots_ee_missdat.RData"))


