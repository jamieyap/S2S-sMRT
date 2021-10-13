# -----------------------------------------------------------------------------
# Specify data generating parameters: Volume of data to generate
# -----------------------------------------------------------------------------

N_sim <- 5  # Total number of simulated datasets
N_participants <- 75  # Total number of participants
tot_decision_points <- 720*10  # Total number of decision points
prob_missing <- 0.30 # We will simulate missing-completely-at-random data

# -----------------------------------------------------------------------------
# Specify data generating parameters: Parameters governing the relationship
# among random variables
# -----------------------------------------------------------------------------

prob_stressed <- 0.49
prob_not_stressed <- 0.50
prob_active <- 0.01

# Constant throughout all decision points classified as stressed
prob_coin_flip_stressed <- 0.001
# Constant throughout all decision points classified as not stressed
prob_coin_flip_not_stressed <- 0.001

# -----------------------------------------------------------------------------
# Specify data analysis parameters
# -----------------------------------------------------------------------------
tot_excursion_length <- 120  # Total length of excursion, e.g., 120 decision points after the k^{th} decision point

