library(Matrix)
library(parallel)
source("paths.R")
source("params.R")

# -----------------------------------------------------------------------------
# Commentary on params required from params.R
# -----------------------------------------------------------------------------

# N_sim
# N_participants
# tot_decision_points 
# prob_missing
# 
# prob_stressed 
# prob_active
# prob_not_stressed 
#
# prob_coin_flip_stressed
# prob_coin_flip_not_stressed 

# -----------------------------------------------------------------------------
# Primitive functions for generating data
# -----------------------------------------------------------------------------

GenerateParticipant <- function(participant_id,
                                tot_decision_points,
                                prob_missing,
                                prob_stressed,
                                prob_not_stressed,
                                prob_active,
                                prob_coin_flip_stressed,
                                prob_coin_flip_not_stressed){
  
  # Simulate the outcome, assuming that it will be observed at all DP's
  dat_sample <- rmultinom(n = tot_decision_points, 
                          size = 1, 
                          prob = c(prob_stressed, 
                                   prob_not_stressed,
                                   prob_active)
                          )
  
  is_stressed_now <- dat_sample[1,] 
  is_not_stressed_now <- dat_sample[2,]
  is_active_now <- dat_sample[3,] 
  
  Yk <- 1*(is_stressed_now==1) + 2*(is_not_stressed_now) + 3*(is_active_now==1)
  
  # Create stratification variable
  Xk <- Yk
  # When classification is 'physically active', set the value of Xk to be NA
  Xk <- replace(Xk, Xk==3, NA_real_)  
  # Complete construction of the variable Xk
  Xk <- replace(Xk, Xk==2, 0)
  
  # Create treatment eligibility indicator 
  Ik <- ifelse(Yk==3, 0, 1)
  
  # Simulate randomization assignment
  probAk <- ifelse(Xk==1, prob_coin_flip_stressed, prob_coin_flip_not_stressed)
  Ak <- lapply(probAk, 
               function(use_this_prob){
                 ifelse(is.na(use_this_prob), 
                        NA, 
                        rbinom(n = 1, size = 1, prob = use_this_prob)
                 )
               }
  )
  Ak <- unlist(Ak)
  
  # Create observed data indicator
  # The manner by which we generate Ok is independent of all
  # the other variables, i.e., MCAR
  Ok <- rbinom(n = tot_decision_points, size = 1, prob = (1-prob_missing))
  
  # Which among the Yk's will actually be observed for this participant?
  Yobsk <- ifelse(Ok==1, Yk, NA_real_)
  Xobsk <- ifelse(Ok==1, Xk, NA_real_)
  
  # Create other remaining variables
  dp <- 1:tot_decision_points
  id <- rep(x = participant_id, times = tot_decision_points)
  
  # Store simulated data into a Matrix
  dat_participant <- Matrix::Matrix(data = cbind(id, dp, Yk, Xk, Ik, probAk, Ak, Ok, Yobsk, Xobsk))
  
  return(dat_participant)
}

# -----------------------------------------------------------------------------
# Now, let's generate simulated datasets for a large number of participant-days
# -----------------------------------------------------------------------------

cl <- makeCluster(getOption("cl.cores", detectCores()-1))
clusterSetRNGStream(cl = cl, iseed = 73104273)
clusterExport(cl, c("GenerateParticipant",
                    "N_participants",
                    "N_sim",
                    "tot_decision_points",
                    "prob_missing",
                    "prob_stressed",
                    "prob_not_stressed",
                    "prob_active",
                    "prob_coin_flip_stressed",
                    "prob_coin_flip_not_stressed"))

simlist <- list()
for(i in 1:N_sim){
  mylist <- parLapply(cl = cl,
                      X = 1:N_participants, 
                      fun = GenerateParticipant,
                      tot_decision_points = tot_decision_points,
                      prob_missing = prob_missing,
                      prob_stressed = prob_stressed,
                      prob_not_stressed = prob_not_stressed,
                      prob_active = prob_active,
                      prob_coin_flip_stressed = prob_coin_flip_stressed,
                      prob_coin_flip_not_stressed = prob_coin_flip_not_stressed)
  
  mydat <- do.call(rbind, mylist)
  simnum <- rep(i, nrow(mydat))
  mydat <- cbind(simnum, mydat)
  simlist <- append(simlist, list(mydat))
}

stopCluster(cl)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------
save(simlist, all_params,
     file = file.path(path_staged_data, "mcar.RData"))

