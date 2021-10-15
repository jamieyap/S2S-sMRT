library(Matrix)
library(parallel)
source("paths.R")
source("params.R")
load(file.path(path_staged_data, "allroots_ee_rho.RData"))
load(file.path(path_staged_data, "allroots_ee_missdat.RData"))
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
  RHO0 <- allroots_ee_rho[idx_sim,"RHO0"]
  RHO1 <- allroots_ee_rho[idx_sim,"RHO1"]
  
  # Note: If Ik==1, it is possible for Xk to be a missing value
  # Note: If Xobsk is missing (i.e., has a value of NA)
  # then cAk will take on a missing value (i.e., NA) as well
  cAk <- ifelse(dat[,"Xobsk"]==0, 
                dat[,"Ak"] - RHO0,
                dat[,"Ak"] - RHO1)
  
  # Inverse probability of observing the trichotomous outcome at decision point k
  PSI0 <- allroots_ee_missdat[idx_sim, "PSI0"]
  ETA0 <- allroots_ee_missdat[idx_sim, "ETA0"]
  IPOk <- exp(-PSI0 - dat[,"Ak"] * ETA0)
  
  dat <- cbind(dat, cAk = cAk, IPOk = IPOk)
  weightedsimlist[[idx_sim]] <- dat
}

# -----------------------------------------------------------------------------
# Define estimating equation
# -----------------------------------------------------------------------------

ModelEERiskRatio <- function(dat, 
                             values,
                             tot_decision_points,
                             tot_excursion_length){
  
  N_participants <- length(unique(dat[,"id"]))
  ALPHA1 <- values[1]
  ALPHA2 <- values[2]
  BETA11 <- values[3]
  BETA10 <- values[4]
  BETA21 <- values[5]
  BETA20 <- values[6]
  
  running_total_block1 <- 0
  running_total_block2 <- 0
  running_total_block3 <- 0
  running_total_block4 <- 0
  running_total_block5 <- 0
  running_total_block6 <- 0
  
  for(idx_person in 1:N_participants){
    
    # Select rows corresponding to one particular person
    dat_person <- dat[which(dat[,"id"] == idx_person),]
    
    # Step 1: Calculate Y_{t+m} for all m = 1, 2, 3, ..., tot_excursion_length
    columns_copied <- rep(list(dat_person[,"Yobsk"]), tot_excursion_length)
    columns_rolled <- mapply(FUN = tail, 
                             x = columns_copied, 
                             n = -c(1:tot_excursion_length), 
                             SIMPLIFY = FALSE)
    columns_rolled <- mapply(FUN = function(v,m){c(v, rep(NA,m))}, 
                             v = columns_rolled, 
                             m = c(1:tot_excursion_length), 
                             SIMPLIFY = TRUE)
    dimnames(columns_rolled) <- list(NULL, paste("Yobsk", "plus", 1:tot_excursion_length, sep="_"))
    
    # Step 2: When is Y_{t+m}==1? When is Y_{t+m}==2?
    mat_indicator1 <- (columns_rolled==1) 
    mat_indicator2 <- (columns_rolled==2) 
    
    # Step 3: Calculate Rjkm for j=1 and j=2
    R1km <- exp(-dat_person[,"Ak"] * (dat_person[,"Xobsk"] * BETA11 + (1 - dat_person[,"Xobsk"]) * BETA10)) * mat_indicator1 - exp(ALPHA1)
    R2km <- exp(-dat_person[,"Ak"] * (dat_person[,"Xobsk"] * BETA21 + (1 - dat_person[,"Xobsk"]) * BETA20)) * mat_indicator2 - exp(ALPHA2)
    
    # Step 4: Calculate O_{k+m} for all m = 1, 2, 3, ..., tot_excursion_length
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
    Okm <- columns_rolled
    
    # Step 6: Other remaining terms to calculate
    COMMON_TERM <- dat_person[,"Wk"] * dat_person[,"IPOk"]
    
    # Step 7: Proceed to block-specific calculations.
    # Note that at this step, Ik and Okm have not yet been explicitly incorporated 
    # into the block-specific calculations; they will be incorporated at the next two steps
    BLOCK1 <- COMMON_TERM * R1km
    BLOCK2 <- COMMON_TERM * R2km
    BLOCK3 <- COMMON_TERM * R1km * dat_person[,"cAk"] * dat_person[,"Xobsk"]
    BLOCK4 <- COMMON_TERM * R2km * dat_person[,"cAk"] * (1 - dat_person[,"Xobsk"])
    BLOCK5 <- COMMON_TERM * R1km * dat_person[,"cAk"] * dat_person[,"Xobsk"]
    BLOCK6 <- COMMON_TERM * R2km * dat_person[,"cAk"] * (1 - dat_person[,"Xobsk"])
    
    # Step 8: Determine which rows (i.e., which among t=1,2,3,...,T) to use in the sum for each block
    
    # ---------------------------------------------------------------------------
    # Rows which correspond to decision points for which Ik==0 or 
    # for which Ik==1 and Ok==0 will not be utilized
    # Equivalently, only rows corresponding to decision points for which 
    # Ik==1 and Ok==1 will be utilized
    # ---------------------------------------------------------------------------
    keep_these_rows <- (dat_person[,"Ik"]==1 & dat_person[,"Ok"]==1)
    
    # ---------------------------------------------------------------------------
    # Rows which correspond to decision points between T-M and T 
    # will not be utilized. Equivalently, only rows which correspond to 
    # decision points between1 and T-M-1 will be utilized
    # ---------------------------------------------------------------------------
    all_row_names <- paste((1:tot_decision_points))
    valid_row_names <- paste(1:(tot_decision_points - tot_excursion_length))
    valid_decision_points <- (all_row_names %in% valid_row_names)
    
    # ---------------------------------------------------------------------------
    # Drop all rows (i.e., decision points) which will not be utilized
    # to calculate block-specific sums
    # ---------------------------------------------------------------------------
    BLOCK1 <- BLOCK1[valid_decision_points & keep_these_rows,]
    BLOCK2 <- BLOCK2[valid_decision_points & keep_these_rows,]
    BLOCK3 <- BLOCK3[valid_decision_points & keep_these_rows,]
    BLOCK4 <- BLOCK4[valid_decision_points & keep_these_rows,]
    BLOCK5 <- BLOCK5[valid_decision_points & keep_these_rows,]
    BLOCK6 <- BLOCK6[valid_decision_points & keep_these_rows,]
    Okm <- Okm[valid_decision_points & keep_these_rows,]
    
    # Step 9: Determine which columns (i.e., which among m=1,2,3,...,M) to use in the sum for each block
    BLOCK1 <- ifelse(Okm == 0, 0, BLOCK1)
    BLOCK2 <- ifelse(Okm == 0, 0, BLOCK2)
    BLOCK3 <- ifelse(Okm == 0, 0, BLOCK3)
    BLOCK4 <- ifelse(Okm == 0, 0, BLOCK4)
    BLOCK5 <- ifelse(Okm == 0, 0, BLOCK5)
    BLOCK6 <- ifelse(Okm == 0, 0, BLOCK6)
    
    # Step 10: Get total for this particular participant
    tot_by_dp_block1 <- rowSums(BLOCK1)
    tot_by_dp_block2 <- rowSums(BLOCK2)
    tot_by_dp_block3 <- rowSums(BLOCK3)
    tot_by_dp_block4 <- rowSums(BLOCK4)
    tot_by_dp_block5 <- rowSums(BLOCK5)
    tot_by_dp_block6 <- rowSums(BLOCK6)
    
    current_participant_total_block1 <- sum(tot_by_dp_block1)
    current_participant_total_block2 <- sum(tot_by_dp_block2)
    current_participant_total_block3 <- sum(tot_by_dp_block3)
    current_participant_total_block4 <- sum(tot_by_dp_block4)
    current_participant_total_block5 <- sum(tot_by_dp_block5)
    current_participant_total_block6 <- sum(tot_by_dp_block6)
    
    running_total_block1 <- running_total_block1 + current_participant_total_block1
    running_total_block2 <- running_total_block2 + current_participant_total_block2
    running_total_block3 <- running_total_block3 + current_participant_total_block3
    running_total_block4 <- running_total_block4 + current_participant_total_block4
    running_total_block5 <- running_total_block5 + current_participant_total_block5
    running_total_block6 <- running_total_block6 + current_participant_total_block6
  }
  
  output <- c(running_total_block1 / N_participants,
              running_total_block2 / N_participants,
              running_total_block3 / N_participants,
              running_total_block4 / N_participants,
              running_total_block5 / N_participants,
              running_total_block6 / N_participants)
  
  return(output)
}

# -----------------------------------------------------------------------------
# Get roots using each simulated dataset
# -----------------------------------------------------------------------------

cl <- makeCluster(getOption("cl.cores", detectCores()-1))
clusterSetRNGStream(cl = cl, iseed = 174125)
clusterExport(cl, c("ModelEERiskRatio",
                    "weightedsimlist",
                    "tot_decision_points",
                    "tot_excursion_length"))
clusterEvalQ(cl, {
  library(Matrix)
  library(rootSolve)
})

list_allroots_ee_rr <- parLapply(cl = cl, 
                                 X = weightedsimlist,
                                 fun = function(current_simdat){
                                   allroots_ee <- multiroot(f = ModelEERiskRatio, 
                                                            start = c(0,0,0,0,0,0), 
                                                            dat = current_simdat,
                                                            tot_decision_points = tot_decision_points,
                                                            tot_excursion_length = tot_excursion_length)
                                   simnum <- as.numeric(current_simdat[1,"simnum"])
                                   estimates <- allroots_ee[["root"]]
                                   pardat <- c(simnum, estimates)
                                   return(pardat)
                                   })

stopCluster(cl)

allroots_ee_rr <- do.call(rbind, list_allroots_ee_rr)
dimnames(allroots_ee_rr) <- list(NULL, c("simnum", "ALPHA1", "ALPHA2", "BETA11", "BETA10", "BETA21", "BETA20"))
allroots_ee_rr <- Matrix::Matrix(allroots_ee_rr)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

print(allroots_ee_rr)

save(allroots_ee_rr, all_params,
     file = file.path(path_staged_data, "allroots_ee_rr.RData"))


