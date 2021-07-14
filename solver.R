library(dplyr)
library(rootSolve)

# Change to appropriate RData file
load(file = "simulated_datasets/simdat02.RData")

# -----------------------------------------------------------------------------
# Input parameters specified by the end-user
# -----------------------------------------------------------------------------

# total number of participants
N <- length(unique(dat_all$id)) 
# total number of possible decision points within a participant-day
tot_dp <- length(unique(dat_all$decision_point))
# the maximum value of m (must use m>0)
window_length <- 3 
# do not count intercept term
num_covariates_miss_data_model <- 1 
# note that there are instances when we might want to exclude the first 
# few decision points from estimation, e.g., if we require variables that
# characterize some aspect of past history up to the current decision point
# simply set last_history_dp=0 to avoid this behavior
last_history_dp <- 0

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

ConstructLaggedVariable <- function(dat, prefix, max_look_ahead){
  
  arr <- dat[[prefix]]
  for(idx_window in 1:max_look_ahead){
    curr_lagged_var <- c(tail(arr, n=-idx_window), rep(NA, idx_window))
    dat[[paste(prefix, "_", idx_window, sep="")]] <- curr_lagged_var
  }
  
  return(dat)
}

# -----------------------------------------------------------------------------
# For each decision point t, construct missing data indicators for t+m
# -----------------------------------------------------------------------------

list_dat_all <- list()
for(idx_participant in 1:N){
  curr_dat <- dat_all %>% filter(id==idx_participant)
  list_dat_all <- append(list_dat_all, list(curr_dat))
}

list_dat_all <- lapply(list_dat_all, 
                       ConstructLaggedVariable,
                       prefix = "M_now", 
                       max_look_ahead = window_length)

# See example output of ConstructLaggedVariable
print(list_dat_all[[3]])

# -----------------------------------------------------------------------------
# Construct second component of weight
# -----------------------------------------------------------------------------

list_dat_all <- lapply(list_dat_all, 
                       ConstructLaggedVariable,
                       prefix = "A_now", 
                       max_look_ahead = window_length)

list_dat_all <- lapply(list_dat_all, 
                       ConstructLaggedVariable,
                       prefix = "prob_A_now", 
                       max_look_ahead = window_length)

list_dat_all <- lapply(list_dat_all, 
                       ConstructLaggedVariable,
                       prefix = "Y_now", 
                       max_look_ahead = window_length)

list_dat_all <- lapply(list_dat_all, 
                       ConstructLaggedVariable,
                       prefix = "I_now", 
                       max_look_ahead = window_length)

list_dat_all <- lapply(list_dat_all, 
                       function(this_dat, win_len = window_length){
                         
                         dat_A <- this_dat %>% select(starts_with("A_now_")) 
                         dat_A <- abs(dat_A - 1)
                         dat_prob_A <- this_dat %>% select(starts_with("prob_A_now_"))
                         dat_ratio <- dat_A/(1 - dat_prob_A)
                         colnames(dat_ratio) <- paste("ratio", 1:window_length, sep="_")
                         
                         dat_I <- this_dat %>% select(starts_with("I_now_"))
                         which_no_rand <- (dat_I == 0)
                         for(idx_col in 1:window_length){
                           dat_ratio[, idx_col] <- if_else(which_no_rand[,idx_col], 0, dat_ratio[, idx_col])
                         }
                         
                         this_dat[["W2_now"]] <- apply(dat_ratio, 1, prod)
                         this_dat[["weight_now"]] <- this_dat[["W1_now"]] * this_dat[["W2_now"]]
                           
                         return(this_dat)
                         })

# -----------------------------------------------------------------------------
# Only include those decision points which will be used to estimate the 
# treatment effect
# -----------------------------------------------------------------------------

dat_all <- do.call(rbind, list_dat_all)
dat_all <- dat_all %>%
  filter((decision_point > last_history_dp) & (decision_point <= tot_dp - window_length))

list_dat_all <- list()
for(idx_participant in 1:N){
  curr_dat <- dat_all %>% filter(id==idx_participant)
  list_dat_all <- append(list_dat_all, list(curr_dat))
}

# -----------------------------------------------------------------------------
# Collect value of inputs in a list
# -----------------------------------------------------------------------------

use_params <- list()

use_params[["tot_participants"]] <- N
use_params[["window_length"]] <- window_length

# psi and eta are parameters in our model for the missing data indicator
# We add one to num_covariates_miss_data_model in order to 
# account for the intercept term
# Note: as of now, the code implementation expects no NA values in 
# M_now and all variables prefixed by M_now, e.g., M_now_1, M_now_2, ...
use_params[["dim_psi"]] <- 1 + num_covariates_miss_data_model
use_params[["dim_eta"]] <- use_params[["dim_psi"]]

# Drop DPs which will not be used to estimate the treatment effect
use_params[["tot_decision_points"]] <- tot_dp
use_params[["begin_after_dp"]] <- last_history_dp

# -----------------------------------------------------------------------------
# Set up data to be in a structure with which we can conveniently perform
# matrix operations
# -----------------------------------------------------------------------------

use_params[["list_M_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(starts_with("M_now_")) %>% t(.)
  return(out_matrix)
})

use_params[["list_A_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(A_now) %>% t(.) 
  return(out_matrix)
})

use_params[["list_cA_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(cA_now) %>% t(.) 
  return(out_matrix)
})

use_params[["list_I_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(I_now) %>% t(.) 
  return(out_matrix)
})

use_params[["list_W_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(weight_now) %>% t(.) 
  return(out_matrix)
})

use_params[["list_Z_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% mutate(ones=1) %>% select(ones, starts_with("Z")) %>% t(.)
  return(out_matrix)
})

# -----------------------------------------------------------------------------
# Specify estimating equation for model of missing data indicator
# -----------------------------------------------------------------------------

EstimatingEquationMissingData <- function(theta, params){
  
  tot_participants <- params[["tot_participants"]]
  tot_decision_points <- params[["tot_decision_points"]]
  window_length <- params[["window_length"]]
  
  dim_psi <- params[["dim_psi"]]
  dim_eta <- params[["dim_eta"]]
  psi <- as.matrix(theta[1:dim_psi])
  eta <- as.matrix(theta[(dim_psi+1):length(theta)])
  max_count_dp <- params[["tot_decision_points"]] - params[["window_length"]] - params[["begin_after_dp"]]
  
  list_cumsum_Utm <- list()
  
  for(idx_participant in 1:tot_participants){
    Z <- params[["list_Z_matrix"]][[idx_participant]]
    A <- params[["list_A_matrix"]][[idx_participant]]
    cA <- params[["list_cA_matrix"]][[idx_participant]]
    I <- params[["list_I_matrix"]][[idx_participant]]
    W <- params[["list_W_matrix"]][[idx_participant]]
    M <- params[["list_M_matrix"]][[idx_participant]]
    
    for(idx_dp in 1:max_count_dp){
      cumsum_Utm <- as.matrix(rep(0, dim_psi + dim_eta))
      
      if(I[,idx_dp] == 1){
        
        colvec <- as.matrix(c(Z[,idx_dp], cA[,idx_dp] * Z[,idx_dp]))
        blip_down <- exp(-(A[,idx_dp] * (t(Z[,idx_dp]) %*% eta)))
        blip_down <- c(blip_down)
        
        for(idx_window in 1:window_length){
          R_tm <- M[idx_window, idx_dp] - exp((t(Z[,idx_dp]) %*% psi) + A[,idx_dp] * (t(Z[,idx_dp]) %*% eta))
          R_tm <- c(R_tm)
          # Note that all the following are scalars: blip_down , I[,idx_dp] , W[,idx_dp] , R_tm
          U_tm <- blip_down * I[,idx_dp] * W[,idx_dp] * R_tm * colvec
          cumsum_Utm <- cumsum_Utm + U_tm
        }
      }
    }
    
    list_cumsum_Utm <- append(list_cumsum_Utm, list(cumsum_Utm))
  }
  
  average_cumsum_Utm <- do.call(cbind, list_cumsum_Utm)
  average_cumsum_Utm <- rowMeans(average_cumsum_Utm)
  average_cumsum_Utm <- c(average_cumsum_Utm)
  
  return(average_cumsum_Utm)
}

# -----------------------------------------------------------------------------
# Use rootSolve::multiroot to estimate the model for missing data indicator
# -----------------------------------------------------------------------------

# Initial value of theta
theta_init <- rep(0, use_params[["dim_psi"]] + use_params[["dim_eta"]])

# Estimate psi and eta
result <- multiroot(f = EstimatingEquationMissingData, start = theta_init, params = use_params)

# Print results on raw scale
print(result$root)

# Print result on exp scale
print(exp(result$root))



