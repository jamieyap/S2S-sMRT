library(dplyr)
set.seed(3857990)

# -----------------------------------------------------------------------------
# Specify data generating parameters
# -----------------------------------------------------------------------------

N_participants <- 1000  # total number of participants
tot_decision_points <- 10  # total number of decision points

prob_stressed <- 0.15
prob_active <- 0.25
prob_not_stressed <- 1 - (prob_stressed + prob_active)

prob_missing <- 0.20

r_11 <- 0.4/6
r_21 <- 0.55/6 
r_31 <- 0.8/6 
r_10 <- 1/6
r_20 <- 0.5/6 
r_30 <- 1 - (r_11 + r_21 + r_31 + r_10 + r_20)

list_datagen_params <- list(N_participants = N_participants,
                            tot_decision_points = tot_decision_points,
                            prob_stressed = prob_stressed,
                            prob_active = prob_active,
                            prob_not_stressed = prob_not_stressed,
                            prob_missing = prob_missing,
                            r_11 = r_11,
                            r_21 = r_21,
                            r_31 = r_31,
                            r_10 = r_10,
                            r_20 = r_20,
                            r_30 = r_30)

# -----------------------------------------------------------------------------
# Begin to simulate data for all participant-decision points
# -----------------------------------------------------------------------------

list_all <- list()

for(i in 1:N_participants){
  # Generate data for just one participant
  dat_sample <- rmultinom(n = tot_decision_points, size = 1, prob = c(prob_stressed, prob_active, prob_not_stressed))
  dat_observed <- data.frame(id = i, 
                             decision_point = 1:tot_decision_points, 
                             is_stressed_now = dat_sample[1,], 
                             is_active_now = dat_sample[2,], 
                             is_not_stressed_now = dat_sample[3,])
  
  # Create variables for the current decision point
  dat_observed <- dat_observed %>%
    mutate(I_now = if_else(is_active_now == 0, 1, 0)) %>%
    mutate(Y_now = NA_real_) %>%
    mutate(Y_now = replace(Y_now, is_stressed_now==1, 1)) %>%
    mutate(Y_now = replace(Y_now, is_active_now==1, 2)) %>%
    mutate(Y_now = replace(Y_now, is_not_stressed_now==1, 3)) %>%
    mutate(X_now = NA_real_) %>%
    mutate(X_now = replace(X_now, Y_now == 1, 1)) %>%
    mutate(X_now = replace(X_now, Y_now == 3, 0))
  
  # Create lagged variables
  dat_observed <- dat_observed %>%
    mutate(Y_past = c(NA, head(Y_now, n=-1)))
  
  # What are the randomization probabilities?
  dat_observed <- dat_observed %>%
    mutate(prob_A_now = case_when(
      Y_past == 1 & X_now == 1 ~ r_11,
      Y_past == 2 & X_now == 1 ~ r_21,
      Y_past == 3 & X_now == 1 ~ r_31,
      Y_past == 1 & X_now == 0 ~ r_10,
      Y_past == 2 & X_now == 0 ~ r_20,
      Y_past == 3 & X_now == 0 ~ r_30,
      TRUE ~ NA_real_
    ))
  
  # Determine treatment assignment
  dat_observed <- dat_observed %>% 
    mutate(tmp_prob_A_now = if_else(is.na(prob_A_now), 0, prob_A_now),
           A_now = NA_real_)
  
  dat_observed[["A_now"]] <- lapply(dat_observed[["tmp_prob_A_now"]], function(curr_prob){
    this_A_now <- rbinom(n = 1, size = 1, prob = curr_prob)
    return(this_A_now)
  })
  
  dat_observed <- dat_observed %>% 
    select(-tmp_prob_A_now) %>%
    mutate(A_now = replace(A_now, I_now == 0 | decision_point == 1, NA_real_))
  
  # Determine whether indicator for stress/nostress/physically active is missing
  M_now <- rbinom(n = tot_decision_points, size = 1, prob = (1 - prob_missing))
  
  # Update data frame
  dat_observed <- dat_observed %>% 
    mutate(M_now = M_now) %>%
    mutate(Y_star_now = if_else(M_now==1, Y_now, NA_real_))
  
  list_all <- append(list_all, list(dat_observed))
}

dat_all <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# Save simulated dataset and data generating parameters
# -----------------------------------------------------------------------------
save(dat_all, list_datagen_params, file = "simulated_datasets/simdat.RData")

