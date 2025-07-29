
source("1. functions.R")
load("data/data_ready.RData")


library(dplyr)
library(pROC)
library(scales)
library(ggplot2)


options(scipen=999)


# Outlier analysis ####

recommended_data <- data_main[data_main$decision_binary_factor==1,]

# Calculate modified Z-scores for icer_aip
recommended_data$mod_z_icer <- calculate_modified_z_scores(recommended_data$ICER)

# Identify outliers using a threshold of 3.5
outlier_threshold <- 3.5

hist_z_ICER <- ggplot(recommended_data, aes(x = mod_z_icer)) +
  geom_histogram(bins = 30, fill = "#8AB3DC", color = "#1E4164") +
  geom_vline(xintercept = c(-outlier_threshold, outlier_threshold), linetype = "dashed", color = "#FF899C") +
  labs(title = "Distribution of Modified Z-Scores for ICER AIP",
       x = "Modified Z-Score",
       y = "Frequency") +
  theme_classic(base_size=10) +
  theme(
    legend.position = "none",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )

# If you want to see details about the extreme outliers:
outliers <- recommended_data[abs(recommended_data$mod_z_icer) > outlier_threshold, ]

data_main_all <- data_main
# Now exclude the outliers from data_main
data_main <- data_main[!(data_main$icer_id %in% outliers$icer_id), ]



# Descriptives ####

## ICER distribution ####

ICERsplot <- gg_outlier_bin_grouped(x = data_main,
                                    var_name = "ICER",
                                    group_var = "decision_binary_factor",
                                    cut_off_floor = NA,
                                    cut_off_ceiling = 3000000,
                                    col = NA,
                                    fill_regular_0 = "#1E4164",      # 0 = red
                                    fill_regular_1 = "#8AB3DC",    # 1 = green
                                    fill_outliers = "#FFB5C1",      # outliers = grey
                                    binwidth = 30000) +
  labs(
    x = "ICER (€)",
    y = "Frequency") +
  scale_fill_manual(values = c("0" = "#1E4164", "1" = "#8AB3DC"),
                    labels = c("0" = "Not recommended", "1" = "Recommended"),
                    name = "Decision") +
  scale_x_continuous(
    breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000), # Custom breaks
    labels = scales::dollar_format(prefix = "€", big.mark = ",", decimal.mark = ".")
  )+
  theme_classic(base_size=10) +
  theme(
    legend.position = "none",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )


## Patient number distribution ####


pnplot <- ggplot(data_main, aes(x = patient_numbers, fill = decision_binary_factor)) +
  geom_histogram(binwidth = 10,
                 boundary = 0,
                 position = "dodge") +
  scale_fill_manual(values = c("0" = "#1E4164", "1" = "#8AB3DC"),
                    labels = c("0" = "Not recommended", "1" = "Recommended"),
                    name = "Decision") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +  # Adjust the range as needed
  labs(x = "Number of patients",
       y = "Frequency") +
  theme_classic(base_size=10) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )



# Primary analysis ####

# Create logistic regression model with scaled ICER
model_basecase <- glm(
  decision_binary_factor ~ ICER_10k,  # Now coefficient represents change per €10,000
  family = binomial(link = "logit"),
  data = data_main
)


primary_analysis <- calculate_optimal_threshold_log(model = model_basecase,
                                                    data  = data_main,
                                                    icer_col     = "ICER_10k",
                                                    decision_col = "decision_binary_factor")

# Create plot without axes first
plot(primary_analysis$roc_curve,
     main        = "ROC Curve",
     xlab        = "False Positive Rate",
     ylab        = "True Positive Rate",
     print.auc   = TRUE,
     legacy.axes = TRUE,
     axes        = FALSE)

primary_log_class_model <- primary_analysis$logistic_plot +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +theme_classic(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )


# Secondary analysis ####


# Oncology indications ####
model_onco <- glm(decision_binary_factor ~ ICER_10k,
                  family = binomial(link = "logit"),
                  data = data_onco)

sec_analysis_onco <- calculate_optimal_threshold_log(model = model_onco,
                                                     data = data_onco,
                                                     icer_col = "ICER_10k",
                                                     decision = "decision_binary_factor")
# Create plot without axes first
plot(sec_analysis_onco$roc_curve,
     main        = "ROC Curve",
     xlab        = "False Positive Rate",
     ylab        = "True Positive Rate",
     print.auc   = TRUE,
     legacy.axes = TRUE,
     axes        = FALSE)

sec_onco_log_class_model <- sec_analysis_onco$logistic_plot +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +theme_classic(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )

## Other disease areas ####

max_x <- max(data_onco[["ICER_10k"]], na.rm = TRUE) * 1.1 # Extend x-axis by 10%


min_x <- min(data_onco[["ICER_10k"]], na.rm = TRUE) * 0.9 # Extend x-axis by 10%


model_non_onco <- glm(decision_binary_factor ~ ICER_10k,
                      family = binomial(link = "logit"),
                      data = data_non_onco)



sec_analysis_non_onco <- calculate_optimal_threshold_log(model = model_non_onco,
                                                         data = data_non_onco,
                                                         icer_col = "ICER_10k",
                                                         decision = "decision_binary_factor",
                                                         max_x = max_x,
                                                         min_x = min_x)
# Create plot without axes first
plot(sec_analysis_non_onco$roc_curve,
     main        = "ROC Curve",
     xlab        = "False Positive Rate",
     ylab        = "True Positive Rate",
     print.auc   = TRUE,
     legacy.axes = TRUE,
     axes        = FALSE)

sec_non_onco_log_class_model <- sec_analysis_non_onco$logistic_plot +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +theme_classic(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )


# Patient numbers ####


# Remove rows with NA in BIM for ROC purposes
data_main_pt_num <- data_main %>%
  filter(!is.na(patient_numbers))

model_pt_num <- glm(decision_binary_factor ~ ICER_10k + patient_numbers,
                    family = binomial(link = "logit"),
                    data   = data_main_pt_num)

model_pt_num_intx <- glm(decision_binary_factor ~ ICER_10k + patient_numbers + ICER_10k*patient_numbers,
                         family = binomial(link = "logit"),
                         data   = data_main_pt_num)

## Patient number without interaction term ####


# Predict probabilities and create ROC curve
probs_covariate <- predict(model_pt_num,
                           type = "response") # Predict the probabilities of the outcome using the model

# Create ROC curve by comparing the predicted probabilities with the actual binary outcome
roc_covariate   <- roc(data_main_pt_num$decision_binary_factor, 
                       probs_covariate) # Use the actual outcome data (from dataset) and predicted probabilities

# Get optimal threshold value using Youden index
coords_covariate <- coords(roc_covariate,
                           "best", 
                           best.method = "youden") # Calculate optimal threshold (using Youden index)

# Extract the optimal threshold value
optimal_threshold_covariate <- coords_covariate$threshold[1] # The best threshold based on Youden index

# Define function to calculate the difference between predicted probability and threshold
diff_func <- function(icer, pt_num, model, threshold) {
  
  # Predict probability for given ICER and BIM values using the provided model
  predicted_prob <- predict(model,
                            newdata = data.frame(ICER_10k = icer,
                                                 patient_numbers = pt_num),
                            type = "response") # Model prediction
  
  # Calculate the difference between predicted probability and threshold
  diff <- predicted_prob - threshold
  return(diff) # Return the difference
}

# Generate a range of BIM values from minimum to maximum (with a step of 10)
pt_range <- seq(from = min(data_main_pt_num$patient_numbers, na.rm = TRUE),
                to = max(data_main_pt_num$patient_numbers, na.rm = TRUE),
                by = 2)

# Pre-define a data frame to store results with budget impact and threshold values
icer_pt_num_results <- data.frame(Patient_number = pt_range,
                                  threshold_covariate = NA)

# Get the maximum and minimum ICER_10k values from the dataset
icer_max <- max(data_main_pt_num$ICER_10k, na.rm = TRUE)
icer_min <- min(data_main_pt_num$ICER_10k, na.rm = TRUE)

# Loop over all BIM values to calculate corresponding willingness-to-pay thresholds
for (i in seq_along(pt_range)) {
  
  # Define function that takes a value and calculates the difference based on ICER and BIM
  f <- function(value) {
    diff_func(value, pt_range[i], model_pt_num, optimal_threshold_covariate)
  }
  
  # Calculate the difference for the minimum and maximum ICER values
  f_min <- f(icer_min)
  f_max <- f(icer_max)
  
  # If the signs of the differences (f_min * f_max) are the same, set threshold to NA (no root found)
  if (f_min * f_max > 0) {
    icer_pt_num_results$threshold_covariate[i] <- NA
  } else {
    # Otherwise, find the ICER threshold using uniroot to find the root of the difference function
    threshold_base <- uniroot(f,
                              interval = c(min(data_main_pt_num$ICER_10k, na.rm = TRUE),
                                           max(data_main_pt_num$ICER_10k, na.rm = TRUE)))$root
    # Store the threshold (scaled by 100,000 for appropriate budget impact units)
    icer_pt_num_results$threshold_covariate[i] <- threshold_base
  }
}

## Patient number with interaction term ####


# Predict probabilities and create ROC curve
probs_covariate_intx <- predict(model_pt_num_intx,
                                type = "response") # Predict the probabilities of the outcome using the model

# Create ROC curve by comparing the predicted probabilities with the actual binary outcome
roc_covariate_intx   <- roc(data_main_pt_num$decision_binary_factor, 
                            probs_covariate_intx) # Use the actual outcome data (from dataset) and predicted probabilities

# Get optimal threshold value using Youden index
coords_covariate_intx <- coords(roc_covariate_intx,
                                "best", 
                                best.method = "youden") # Calculate optimal threshold (using Youden index)

# Extract the optimal threshold value
optimal_threshold_covariate_intx <- coords_covariate_intx$threshold[1] # The best threshold based on Youden index

# Pre-define a data frame to store results with budget impact and threshold values
icer_pt_num_results_intx <- data.frame(Patient_number = pt_range,
                                       threshold_covariate = NA)

# Loop over all BIM values to calculate corresponding willingness-to-pay thresholds
for (i in seq_along(pt_range)) {
  
  # Define function that takes a value and calculates the difference based on ICER and BIM
  f <- function(value) {
    diff_func(value, pt_range[i], model_pt_num_intx, optimal_threshold_covariate_intx)
  }
  
  # Calculate the difference for the minimum and maximum ICER values
  f_min <- f(icer_min)
  f_max <- f(icer_max)
  
  # If the signs of the differences (f_min * f_max) are the same, set threshold to NA (no root found)
  if (f_min * f_max > 0) {
    icer_pt_num_results_intx$threshold_covariate[i] <- NA
  } else {
    # Otherwise, find the ICER_10k threshold using uniroot to find the root of the difference function
    threshold_base <- uniroot(f,
                              interval = c(min(data_main_pt_num$ICER_10k, na.rm = TRUE),
                                           max(data_main_pt_num$ICER_10k, na.rm = TRUE)))$root
    # Store the threshold (scaled by 100,000 for appropriate budget impact units)
    icer_pt_num_results_intx$threshold_covariate[i] <- threshold_base
  }
}

## Plot of both patient number models


#Combine the data and add model labels
icer_pt_num_results$model <- "Model 4"
icer_pt_num_results_intx$model <- "Model 5"

icer_pt_num_results$threshold_covariate <- icer_pt_num_results$threshold_covariate*10000
icer_pt_num_results_intx$threshold_covariate <- icer_pt_num_results_intx$threshold_covariate *10000

combined_results <- rbind(icer_pt_num_results, icer_pt_num_results_intx)

interactionplot <- ggplot(combined_results, aes(x = Patient_number, y = threshold_covariate, linetype = model, color = model)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2, span = 0.3) +  # Smooth curves
  labs(
    x = "Number of patients", 
    y = "Willingness to pay threshold", 
    linetype = "Model",
    color = "Model",
  ) +
  scale_y_continuous(
    labels = function(x) paste0("€", format(x, big.mark = ",", decimal.mark = ".")),
    breaks = pretty_breaks(n = 8)
  ) +
  scale_x_continuous(
    labels = comma_format(),
    breaks = pretty_breaks(n = 6)
  ) +
  scale_color_manual(values = c("#1E4164", "#8AB3DC")) +
  theme_classic(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )

# Sensitivity analyses ####

## Base-case model with outliers ####

model_outliers <- glm(decision_binary_factor ~ ICER_10k,
                      family = binomial(link = "logit"),  # Specify the family for logistic regression
                      data   = data_main_all)  # The data used is the reduced dataset for the scenario

all_data_analysis <- calculate_optimal_threshold_log(model = model_outliers,
                                                     data  = data_main_all,
                                                     icer_col     = "ICER_10k",
                                                     decision_col = "decision_binary_factor")

# Create plot without axes first
plot(all_data_analysis$roc_curve,
     main        = "ROC Curve",
     xlab        = "False Positive Rate",
     ylab        = "True Positive Rate",
     print.auc   = TRUE,
     legacy.axes = TRUE,
     axes        = FALSE)

bc_outliers_log_class_model <- all_data_analysis$logistic_plot +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +theme_classic(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )

## Optimistic scenario ####

data_optimistic <- data_optimistic %>%
  mutate(icer_id = row_number())

recommended_data <- data_optimistic[data_optimistic$decision_binary_factor==1,]

# Calculate modified Z-scores for icer_aip
recommended_data$mod_z_icer <- calculate_modified_z_scores(recommended_data$ICER)

# If you want to see details about the extreme outliers:
outliers <- recommended_data[abs(recommended_data$mod_z_icer) > outlier_threshold, ]

data_optimistic_all <- data_optimistic
# Now exclude the outliers from data_optimistic
data_optimistic <- data_optimistic[!(data_optimistic$icer_id %in% outliers$icer_id), ]

### Optimistic scenario with outliers ####


# Fit the logistic regression model using the reduced dataset for scenario analysis
# The model is fitted without any additional covariates, and the family is set to binomial with logit link function for logistic regression
model_optimistic_all <- glm(decision_binary_factor ~ ICER_10k,
                            family = binomial(link = "logit"),  # Specify the family for logistic regression
                            data   = data_optimistic_all)  # The data used is the reduced dataset for the scenario

optimistic_analysis_all <- calculate_optimal_threshold_log(model = model_optimistic_all,
                                                           data  = data_optimistic_all,
                                                           icer_col     = "ICER_10k",
                                                           decision_col = "decision_binary_factor")

# Create plot without axes first
plot(optimistic_analysis_all$roc_curve,
     main        = "ROC Curve",
     xlab        = "False Positive Rate",
     ylab        = "True Positive Rate",
     print.auc   = TRUE,
     legacy.axes = TRUE,
     axes        = FALSE)

optimistic_all_log_class_model <- optimistic_analysis_all$logistic_plot +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +theme_classic(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )





### Optimistic scenario without outliers ####

model_optimistic <- glm(decision_binary_factor ~ ICER_10k,
                        family = binomial(link = "logit"),  # Specify the family for logistic regression
                        data   = data_optimistic)  # The data used is the reduced dataset for the scenario

optimistic_analysis <- calculate_optimal_threshold_log(model = model_optimistic,
                                                       data  = data_optimistic,
                                                       icer_col     = "ICER_10k",
                                                       decision_col = "decision_binary_factor")

plot(optimistic_analysis$roc_curve,
     main        = "ROC Curve",
     xlab        = "False Positive Rate",
     ylab        = "True Positive Rate",
     print.auc   = TRUE,
     legacy.axes = TRUE,
     axes        = FALSE)

optimistic_log_class_model <- optimistic_analysis$logistic_plot +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +theme_classic(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # removes all legend titles
    panel.grid.major= element_line(color = "grey90", size = 0.5)
  )


