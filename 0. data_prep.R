#Average conversion rate from 10 June 2024 to 9 June 2025
eur_to_dkk <- 7.4602
dkk_to_eur <- 1/eur_to_dkk

load("data.Rdata")

data_formatted <- data  %>%
  mutate(decision_binary_factor = case_when(
    decision == "Not recommended" ~ 0,        # Map "Not recommended" to 0
    decision == "Recommended" ~ 1,            # Map "Recommended" to 1
    decision == "Partially recommended" ~ 1,  # Map "Partially recommended" to 1
    TRUE ~ NA_real_  # For any other value, assign NA
  )) %>%   mutate(decision_binary_factor = factor(decision_binary_factor, levels = c(0, 1))) %>%   # Convert the 'decision_binary_factor' to a factor with two levels: 0 and 1


data_main <- data_formatted %>%
  filter(include_pessimistic == "Y")

data_optimistic <- data_formatted %>%
  filter(include_optimistic == "Y")

# Create oncology binary variable
data_main <- data_main %>%
  mutate(oncology_binary = ifelse(disease_area == "Kræftsygdomme, Cancer", 1, 0))

data_main$ICER_10k <- data_main$ICER / 10000

data_onco <- data_main %>% #Take where disease_area ==1
  filter(data_main$oncology_binary==1)

# # All non-oncology indications
data_non_onco <- data_main %>% #Take where disease_area ==1
  filter(data_main$oncology_binary==0)

save(data_main,
     data_optimistic,
     data_onco,
     data_non_onco,
     file = "data_ready.RData")