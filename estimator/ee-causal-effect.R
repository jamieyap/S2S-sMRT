library(Matrix)
source("paths.R")
source("params.R")
load(file.path(path_staged_data, "allroots_ee_rho.RData"))
load(file.path(path_staged_data, "allroots_ee_missdat.RData"))
load(file.path(path_staged_data, "weightedsimlist.RData"))

for(idx_sim in 1:N_sim){
  
  dat <- weightedsimlist[[idx_sim]]
  N_participants <- length(unique(dat[,"id"]))
  
  ALPHA1 <- 0
  ALPHA2 <- 0
  
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
    
    # Add more here later
  }
}



