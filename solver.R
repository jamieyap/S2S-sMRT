library(dplyr)
library(rootSolve)

load(file = "simulated_datasets/simdat.RData")

params <- list()
params[["tot_participants"]] <- length(unique(dat_all$id))
params[["tot_decision_points"]] <- length(unique(dat_all$decision_point)) - 1
params[["window_length"]] <- 3

dat_all <- dat_all %>% 
  group_by(id) %>%
  mutate(M_now_1 = c(tail(M_now, n=-1), NA),
         M_now_2 = c(tail(M_now, n=-2), NA, NA),
         M_now_3 = c(tail(M_now, n=-3), NA, NA, NA)) %>%
  ungroup(.)

dat_all <- dat_all %>% 
  mutate(cA_now = A_now - prob_A_now) %>%
  filter(decision_point > 1 & decision_point <= params[["tot_decision_points"]] - params[["window_length"]])

list_dat_all <- list()
for(i in 1:params[["tot_participants"]]){
  curr_dat <- dat_all %>% filter(id==i)
  list_dat_all <- append(list_dat_all, list(curr_dat))
}

params[["dim_psi"]] <- 3
params[["dim_eta"]] <- params[["dim_psi"]]
params[["list_Z_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% mutate(ones=1) %>% select(ones, Z1_now, Z2_now) %>% t(.) 
  return(out_matrix)
})
  
params[["list_A_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(A_now) %>% t(.) 
  return(out_matrix)
})

params[["list_cA_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(cA_now) %>% t(.) 
  return(out_matrix)
})

params[["list_I_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(I_now) %>% t(.) 
  return(out_matrix)
})

params[["list_W_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(weight_now) %>% t(.) 
  return(out_matrix)
})

params[["list_M_matrix"]] <- lapply(list_dat_all, function(curr_dat){
  out_matrix <- curr_dat %>% select(M_now_1, M_now_2, M_now_3) %>% t(.)
  return(out_matrix)
})

ModelMissingDataIndicator <- function(theta, params){
  
  tot_participants <- params[["tot_participants"]]
  tot_decision_points <- params[["tot_decision_points"]]
  window_length <- params[["window_length"]]
  
  dim_psi <- params[["dim_psi"]]
  dim_eta <- params[["dim_eta"]]
  psi <- as.matrix(theta[1:dim_psi])
  eta <- as.matrix(theta[(dim_psi+1):length(theta)])
  max_dp <- params[["tot_decision_points"]] - 1 - params[["window_length"]] # Fix this later on
  
  list_cumsum_Utm <- list()
  
  for(idx_participant in 1:tot_participants){
    Z <- params[["list_Z_matrix"]][idx_participant][[1]]
    A <- params[["list_A_matrix"]][idx_participant][[1]]
    cA <- params[["list_cA_matrix"]][idx_participant][[1]]
    I <- params[["list_I_matrix"]][idx_participant][[1]]
    W <- params[["list_W_matrix"]][idx_participant][[1]]
    M <- params[["list_M_matrix"]][idx_participant][[1]]
    
    for(idx_dp in 1:max_dp){
      cumsum_Utm <- 0
      
      if(I[,idx_dp] ==1){
        
        colvec <- as.matrix(c(Z[,idx_dp], cA[,idx_dp] * Z[,idx_dp]))
        blip_down <- exp(-(A[,idx_dp] * (t(Z[,idx_dp]) %*% eta)))
        blip_down <- c(blip_down)
        
        for(idx_window in 1:window_length){
          R_tm <- M[idx_window, idx_dp] - exp((t(Z[,idx_dp]) %*% psi) + A[,idx_dp] * (t(Z[,idx_dp]) %*% eta))
          R_tm <- c(R_tm)
          U_tm <- blip_down * I[,idx_dp] * W[,idx_dp] * R_tm * colvec
          cumsum_Utm <- cumsum_Utm + U_tm
        }
      }
    }
    
    list_cumsum_Utm <- append(list_cumsum_Utm, list(cumsum_Utm))
  }
  
  average_cumsum_Utm <- do.call(cbind, list_cumsum_Utm)
  average_cumsum_Utm <- rowMeans(average_cumsum_Utm)
  
  return(average_cumsum_Utm)
}

# Initial value of theta
theta_init <- rep(0.1, params[["dim_psi"]] + params[["dim_eta"]])

# Estimate eta and theta
multiroot(f = ModelMissingDataIndicator, start = theta_init, params = params)

