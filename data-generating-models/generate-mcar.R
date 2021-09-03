library(Matrix)
library(parallel)
path_staged_data <- Sys.getenv("path_staged_data")

# -----------------------------------------------------------------------------
# Specify data generating parameters: Volume of data to generate
# -----------------------------------------------------------------------------

N_sim <- 3  # Total number of simulated datasets
N_participants <- 100  # Total number of participants
tot_decision_points <- 1000  # Total number of decision points
prob_missing <- 0.20 # We will simulate missing-completely-at-random data

# -----------------------------------------------------------------------------
# Specify data generating parameters: Parameters governing the relationship
# among random variables
# -----------------------------------------------------------------------------

prob_stressed <- 0.20
prob_active <- 0.30
prob_not_stressed <- 1 - (prob_stressed + prob_active)

# Constant throughout all decision points classified as stressed
prob_coin_flip_stressed <- 0.6 
# Constant throughout all decision points classified as not stressed
prob_coin_flip_not_stressed <- 0.3

# -----------------------------------------------------------------------------
# Primitive functions for generating data
# -----------------------------------------------------------------------------

GenerateParticipant <- function(participant_id,
                                tot_decision_points,
                                prob_missing,
                                prob_stressed,
                                prob_active,
                                prob_not_stressed,
                                prob_coin_flip_stressed,
                                prob_coin_flip_not_stressed){
  
  # Simulate the outcome, assuming that it will be observed at all DP's
  dat_sample <- rmultinom(n = tot_decision_points, 
                          size = 1, 
                          prob = c(prob_stressed, 
                                   prob_active, 
                                   prob_not_stressed)
                          )
  
  is_stressed_now <- dat_sample[1,] 
  is_active_now <- dat_sample[2,] 
  is_not_stressed_now <- dat_sample[3,]
  Yk <- 1*(is_stressed_now==1) + 2*(is_active_now==1) + 3*(is_not_stressed_now)
  
  # Create stratification variable
  Xk <- ifelse(Yk==2, NA_real_, Yk)
  Xk <- replace(Xk, Xk==3, 0)
  
  # Create treatment eligibility indicator 
  Ik <- ifelse((Yk==1)|(Yk==3), 1, 0)
  
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
  cAk <- Ak - probAk
  
  # Create observed data indicator
  Ok <- rbinom(n = tot_decision_points, size = 1, prob = (1-prob_missing))
  
  # Which among the Yk's will actually be observed for this participant?
  Ystark <- ifelse(Ok==1, Yk, NA_real_)
  
  # Create other remaining variables
  dp <- 1:tot_decision_points
  id <- rep(x = participant_id, times = tot_decision_points)
  
  # Store simulated data into a Matrix
  dat_participant <- Matrix::Matrix(data = cbind(id, dp, Yk, Xk, Ik, probAk, cAk, Ok, Ystark))
  
  return(dat_participant)
}

# -----------------------------------------------------------------------------
# Now, let's generate simulated datasets for a large number of participant-days
# -----------------------------------------------------------------------------

cl <- makeCluster(getOption("cl.cores", 15))
clusterSetRNGStream(cl = cl, iseed = 174125)
clusterExport(cl, c("GenerateParticipant",
                    "N_participants",
                    "N_sim",
                    "tot_decision_points",
                    "prob_missing",
                    "prob_stressed",
                    "prob_active",
                    "prob_not_stressed",
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
                      prob_active = prob_active,
                      prob_not_stressed = prob_not_stressed,
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
save(simlist,
     file = file.path(path_staged_data, "mcar.RData"))

