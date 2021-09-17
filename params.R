# -----------------------------------------------------------------------------
# Specify data generating parameters: Volume of data to generate
# -----------------------------------------------------------------------------

N_sim <- 100  # Total number of simulated datasets
N_participants <- 100  # Total number of participants
tot_decision_points <- 7200  # Total number of decision points
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
# Specify data analysis parameters
# -----------------------------------------------------------------------------
tot_excursion_length <- 10  # Total length of excursion, e.g., 120 decision points after the k^{th} decision point


