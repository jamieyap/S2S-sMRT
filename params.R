# -----------------------------------------------------------------------------
# Specify data generating parameters: Volume of data to generate
# -----------------------------------------------------------------------------

N_sim <- 100  # Total number of simulated datasets
N_participants <- 75  # Total number of participants
tot_decision_points <- 100  # Total number of decision points
prob_missing <- 0.30 # We will simulate missing-completely-at-random data

# -----------------------------------------------------------------------------
# Specify data generating parameters: Parameters governing the relationship
# among random variables
# -----------------------------------------------------------------------------

prob_stressed <- 0.30
prob_not_stressed <- 0.50
prob_active <- 0.20

# Constant throughout all decision points classified as stressed
prob_coin_flip_stressed <- 0.01
# Constant throughout all decision points classified as not stressed
prob_coin_flip_not_stressed <- 0.01

# -----------------------------------------------------------------------------
# Specify data analysis parameters
# -----------------------------------------------------------------------------

tot_excursion_length <- 10  # Total length of excursion, e.g., 120 decision points after the k^{th} decision point

# -----------------------------------------------------------------------------
# Store in a list
# -----------------------------------------------------------------------------

all_params <- list(N_sim = N_sim,
                   N_participants = N_participants,
                   tot_decision_points = tot_decision_points,
                   prob_missing = prob_missing,
                   prob_stressed = prob_stressed,
                   prob_not_stressed = prob_not_stressed,
                   prob_active = prob_active,
                   prob_coin_flip_stressed = prob_coin_flip_stressed,
                   prob_coin_flip_not_stressed = prob_coin_flip_not_stressed,
                   tot_excursion_length = tot_excursion_length)

