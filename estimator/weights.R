library(Matrix)
source("paths.R")
source("params.R")
load(file.path(path_staged_data, "allroots_ee_rho.RData"))
load(file.path(path_staged_data, "mcar.RData"))

# -----------------------------------------------------------------------------
# Commentary on params required from params.R
# -----------------------------------------------------------------------------

# N_sim
# tot_excursion_length

# -----------------------------------------------------------------------------
# Primitive functions for calculating weights for one particular participant
# -----------------------------------------------------------------------------

WeightParticipant <- function(dat_person,
                              tot_excursion_length,
                              RHO0,
                              RHO1){
  
  # Create placeholder columns
  dat_person <- cbind(dat_person, Wk = c(NA), W0k = c(NA), W1k = c(NA))
  # 'pi' stands for the Greek letter used to represent 'product'
  list_pi_person <- list() 
  
  # Step 1: Calculate I_{t+m} for all m = 1, 2, 3, ..., tot_excursion_length
  columns_copied <- rep(list(dat_person[,"Ik"]), tot_excursion_length)
  columns_rolled <- mapply(FUN = tail, 
                           x = columns_copied, 
                           n = -c(1:tot_excursion_length), 
                           SIMPLIFY = FALSE)
  columns_rolled <- mapply(FUN = function(v,m){c(v, rep(NA,m))}, 
                           v = columns_rolled, 
                           m = c(1:tot_excursion_length), 
                           SIMPLIFY = TRUE)
  dimnames(columns_rolled) <- list(NULL, paste("Ik", "plus", 1:tot_excursion_length, sep="_"))
  columns_rolled <- (columns_rolled==1)  # Note: Check whether I_{t+m}=1
  # For each row (i.e., decision point t), the resulting product will be equal
  # to 1 if I_{t+m}=1 for all m=1,2,3, ..., M, where M is tot_excursion_length
  product_over_excursion <- apply(columns_rolled, 1, prod)
  list_pi_person[["pi_I"]] <- product_over_excursion
  
  # Step 2: Calculate A_{t+m} for all m = 1, 2, 3, ..., M
  # where M is tot_excursion_length
  columns_copied <- rep(list(dat_person[,"Ak"]), tot_excursion_length)
  columns_rolled <- mapply(FUN = tail, 
                           x = columns_copied, 
                           n = -c(1:tot_excursion_length), 
                           SIMPLIFY = FALSE)
  columns_rolled <- mapply(FUN = function(v,m){c(v, rep(NA,m))}, 
                           v = columns_rolled, 
                           m = c(1:tot_excursion_length), 
                           SIMPLIFY = TRUE)
  dimnames(columns_rolled) <- list(NULL, paste("Ak", "plus", 1:tot_excursion_length, sep="_"))
  columns_rolled <- (columns_rolled==0)  # Note: Check whether A_{t+m}=0
  # For each row (i.e., decision point t), the resulting product will be equal
  # to 1 if A_{t+m}=0 for all m=1,2,3,...,M, where M is tot_excursion_length
  product_over_excursion <- apply(columns_rolled, 1, prod)
  list_pi_person[["pi_A"]] <- product_over_excursion
  
  # Step 3: Calculate p_{t+m} for all m = 1, 2, 3, ..., M,
  # where M is tot_excursion_length
  columns_copied <- rep(list(dat_person[,"probAk"]), tot_excursion_length)
  columns_rolled <- mapply(FUN = tail, 
                           x = columns_copied, 
                           n = -c(1:tot_excursion_length), 
                           SIMPLIFY = FALSE)
  columns_rolled <- mapply(FUN = function(v,m){c(v, rep(NA,m))}, 
                           v = columns_rolled, 
                           m = c(1:tot_excursion_length), 
                           SIMPLIFY = TRUE)
  dimnames(columns_rolled) <- list(NULL, paste("probAk", "plus", 1:tot_excursion_length, sep="_"))
  columns_rolled <- 1 - columns_rolled  # Note: Calculate 1-p_{t+m}
  product_over_excursion <- apply(columns_rolled, 1, prod)
  list_pi_person[["pi_probAprime"]] <- product_over_excursion
  
  # Step 4: Identify rows (i.e., decision points) which will have zero weight
  idx_eligible_throughout_excursion <- (list_pi_person[["pi_I"]]>0)
  idx_eligible_current_dp <- (dat_person[,"Ik"]==1 & dat_person[,"Ok"]==1)
  idx_valid_dp <- (paste(1:tot_decision_points) %in% paste(1:(tot_decision_points - tot_excursion_length)))
  idx_eligible <- (idx_valid_dp & idx_eligible_current_dp & idx_eligible_throughout_excursion)
  
  # Step 5: For each row, calculate person's overall product term
  ratio <- list_pi_person[["pi_A"]] / list_pi_person[["pi_probAprime"]]
  pi_overall <- ifelse(idx_eligible, ratio, 0)
  
  # Step 6: Calculate weight at decision points k which are classified as not stressed
  W0k <- ((RHO0)/(dat_person[,"probAk"])^(dat_person[,"Ak"])) * ((1-RHO0)/(1-dat_person[,"probAk"])^(1-dat_person[,"Ak"])) * pi_overall
  dat_person[,"W0k"] <- ifelse(idx_eligible, W0k, 0)
  
  # Step 7: Calculate weight at decision points k which are classified as stressed
  W1k <- ((RHO1)/(dat_person[,"probAk"])^(dat_person[,"Ak"])) * ((1-RHO1)/(1-dat_person[,"probAk"])^(1-dat_person[,"Ak"])) * pi_overall
  dat_person[,"W1k"] <- ifelse(idx_eligible, W1k, 0)
  
  # Step 8: Finally, put everything together
  Wk <- ((dat_person[,"W1k"])^(dat_person[,"Xobsk"])) * ((dat_person[,"W0k"])^(1-dat_person[,"Xobsk"]))
  dat_person[,"Wk"] <- ifelse(idx_eligible, Wk, 0)
  
  # Step 9: Clean up
  dat_person <- dat_person[, !(colnames(dat_person) %in% c("W0k", "W1k"))]
  
  return(dat_person)
}

# -----------------------------------------------------------------------------
# Calculate weights for each simulated dataset
# -----------------------------------------------------------------------------

for(idx_sim in 1:N_sim){
  
  dat <- simlist[[idx_sim]]
  N_participants <- length(unique(dat[,"id"]))
  RHO0 <- allroots_ee_rho[idx_sim,"RHO0"]
  RHO1 <- allroots_ee_rho[idx_sim,"RHO1"]
  
  list_current <- list()
  for(idx_person in 1:N_participants){
    # Take subset of rows corresponding to one particular participant
    these_rows <- (dat[,"id"] == idx_person)
    dat_person <- dat[these_rows,]
    weighteddat_person <- WeightParticipant(dat_person = dat_person,
                                            tot_excursion_length = tot_excursion_length,
                                            RHO0 = RHO0,
                                            RHO1 = RHO1)
    list_current <- append(list_current, weighteddat_person)
  }
  
  simlist[[idx_sim]] <- do.call(rbind, list_current)
}

weightedsimlist <- simlist

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------
save(weightedsimlist,
     file = file.path(path_staged_data, "weightedsimlist.RData"))

