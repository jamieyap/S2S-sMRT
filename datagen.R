library(dplyr)
set.seed(897123)

# -----------------------------------------------------------------------------
# Specify data generating parameters
# -----------------------------------------------------------------------------

N_participants <- 5000  # total number of participants
tot_decision_points <- 30  # total number of decision points

prob_stressed <- 0.15
prob_active <- 0.25
prob_not_stressed <- 1 - (prob_stressed + prob_active)

p_coin_flip_stressed <- 0.6 # constant throughout all DP's classified as stressed
p_coin_flip_not_stressed <- 0.3 # constant throughout all DP's classified as not stressed

# Simulate MCAR
prob_missing <- 0.40

list_datagen_params <- list(N_participants = N_participants,
                            tot_decision_points = tot_decision_points,
                            prob_stressed = prob_stressed,
                            prob_active = prob_active,
                            prob_not_stressed = prob_not_stressed,
                            prob_missing = prob_missing)

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
  
  # What are the randomization probabilities?
  dat_observed <- dat_observed %>%
    mutate(prob_A_now = case_when(
      X_now == 1 ~ p_coin_flip_stressed,
      X_now == 0 ~ p_coin_flip_not_stressed,
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
  
  dat_observed[["A_now"]] <- unlist(dat_observed[["A_now"]])
  dat_observed[["cA_now"]] <- dat_observed[["A_now"]] - dat_observed[["prob_A_now"]]
  
  dat_observed <- dat_observed %>% 
    select(-tmp_prob_A_now) %>%
    mutate(A_now = replace(A_now, I_now == 0, NA_real_))
  
  # Calculate probability of missingness: MCAR
  dat_observed <- dat_observed %>% mutate(prob_M_now = prob_missing) 
  
  # Determine whether indicator for stress/nostress/physically active is missing
  dat_observed <- dat_observed %>% mutate(M_now = NA_real_)
  
  dat_observed[["M_now"]] <- lapply(dat_observed[["prob_M_now"]], function(curr_prob){
    this_M_now <- rbinom(n = 1, size = 1, prob = curr_prob)
    return(this_M_now)
  })
  
  dat_observed[["M_now"]] <- unlist(dat_observed[["M_now"]])
  
  # These are variables which have no relation to the probability of missingness
  Z1_now <- rnorm(n = tot_decision_points)
  Z2_now <- rnorm(n = tot_decision_points)
  
  # Update data frame
  dat_observed <- dat_observed %>% 
    mutate(M_now = M_now,
           Z1_now = Z1_now, 
           Z2_now = Z2_now) %>%
    mutate(Y_star_now = if_else(M_now==1, Y_now, NA_real_))
  
  list_all <- append(list_all, list(dat_observed))
}

dat_all <- do.call(rbind, list_all)

# -----------------------------------------------------------------------------
# Calculate phat(x)
# -----------------------------------------------------------------------------

dat_all  <- dat_all %>% 
  mutate(phat_it_denom_stressed_now = I_now * X_now,
         phat_it_denom_not_stressed_now = I_now * (1 - X_now))

dat_all  <- dat_all %>% 
  mutate(phat_it_numer_stressed_now = I_now * X_now * prob_A_now,
         phat_it_numer_not_stressed_now = I_now * (1 - X_now) * prob_A_now)

dat_phat <- dat_all %>%
  group_by(id, X_now) %>%
  summarise(cumsum_phat_numer_stressed_now = sum(phat_it_numer_stressed_now, na.rm=TRUE),
            cumsum_phat_numer_not_stressed_now = sum(phat_it_numer_not_stressed_now, na.rm=TRUE),
            cumsum_phat_denom_stressed_now = sum(phat_it_denom_stressed_now, na.rm=TRUE),
            cumsum_phat_denom_not_stressed_now = sum(phat_it_denom_not_stressed_now, na.rm=TRUE))

dat_phat_stressed_numer <- dat_phat %>% filter(X_now==1)
phat_stressed_numer <- mean(dat_phat_stressed_numer$cumsum_phat_numer_stressed_now)
dat_phat_stressed_denom <- dat_phat %>% filter(X_now==1)
phat_stressed_denom <- mean(dat_phat_stressed_denom$cumsum_phat_denom_stressed_now)
phat_stressed <- phat_stressed_numer/phat_stressed_denom

print(phat_stressed)

dat_phat_not_stressed_numer <- dat_phat %>% filter(X_now==0)
phat_not_stressed_numer <- mean(dat_phat_not_stressed_numer$cumsum_phat_numer_not_stressed_now)
dat_phat_not_stressed_denom <- dat_phat %>% filter(X_now==0)
phat_not_stressed_denom <- mean(dat_phat_not_stressed_denom$cumsum_phat_denom_not_stressed_now)
phat_not_stressed <- phat_not_stressed_numer/phat_not_stressed_denom

print(phat_not_stressed)

# -----------------------------------------------------------------------------
# Calculate one of the components of the overall weight
# -----------------------------------------------------------------------------

dat_all <- dat_all %>%
  mutate(W1_now = case_when(
    X_now==1 & A_now==1 ~ phat_stressed/prob_A_now,
    X_now==1 & A_now==0 ~ (1-phat_stressed)/(1-prob_A_now),
    X_now==0 & A_now==1 ~ phat_not_stressed/prob_A_now,
    X_now==0 & A_now==0 ~ (1-phat_not_stressed)/(1-prob_A_now),
    TRUE ~ NA_real_
  ))

# -----------------------------------------------------------------------------
# Save simulated dataset and data generating parameters
# -----------------------------------------------------------------------------
dat_all <- dat_all %>%
  select(id, decision_point, 
         I_now, Y_now, X_now, prob_A_now, A_now, cA_now,
         M_now, Y_star_now, W1_now, 
         Z1_now, Z2_now)

save(dat_all, list_datagen_params, file = "simulated_datasets/simdat.RData")

