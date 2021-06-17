library(dplyr)

load(file = "simulated_datasets/simdat.RData")

# -----------------------------------------------------------------------------
# Calculate phat(x) and perform checks against the true value p(x)
# -----------------------------------------------------------------------------


dat_all  <- dat_all %>% 
  mutate(phat_denom_stressed_now = I_now * X_now,
         phat_denom_not_stressed_now = I_now * (1 - X_now))

dat_all  <- dat_all %>% 
  mutate(phat_numer_stressed_now = I_now * X_now * prob_A_now,
         phat_numer_not_stressed_now = I_now * (1 - X_now) * prob_A_now)

# Checks
dat_all %>%
  filter(decision_point > 1) %>%
  group_by(decision_point) %>%
  summarise(sum(phat_denom_stressed_now, na.rm=TRUE)/(N_participants),
            sum(phat_denom_not_stressed_now, na.rm=TRUE)/(N_participants))

dat_all %>%
  filter(decision_point > 1) %>%
  group_by(decision_point) %>%
  summarise(sum(phat_numer_stressed_now, na.rm=TRUE)/(N_participants),
            sum(phat_numer_not_stressed_now, na.rm=TRUE)/(N_participants))


# True value of the denominator
conditional_stressed <- r_11*prob_stressed*prob_stressed + r_21*prob_active*prob_stressed + r_31*prob_not_stressed*prob_stressed
conditional_not_stressed <- r_10*prob_stressed*prob_not_stressed + r_20*prob_active*prob_not_stressed + r_30*prob_not_stressed*prob_not_stressed

print(conditional_stressed)
print(conditional_not_stressed)




