### Calculate modified Z-scores for the icer_aip variable ----------------------
calculate_modified_z_scores <- function(x) {
  # Calculate median
  median_x <- median(x, na.rm = TRUE)
  
  # Calculate MAD (Median Absolute Deviation)
  mad_x <- mad(x, constant = 1.4826, na.rm = TRUE)
  
  # Handle case where MAD is zero
  if (mad_x == 0) {
    return(rep(0, length(x)))
  }
  
  # Calculate modified Z-scores
  modified_z <- 0.6745 * (x - median_x) / mad_x
  return(modified_z)
}



calculate_optimal_threshold_log <- function(model, data, icer_col, decision_col, max_x = NULL, min_x = NULL) {
  # If max_x is null, then take the max_x from the data
  if (is.null(max_x)) {
    max_x <- max(data[[icer_col]], na.rm = TRUE) * 1.1 # Extend x-axis by 10%
  }
  
  if (is.null(min_x)) {
    min_x <- min(data[[icer_col]], na.rm = TRUE) * 0.9 # Extend x-axis by 10%
  }
  
  # Use the provided model directly (no log transformation)
  # Predict probabilities using the model
  probs <- predict(model, type = "response")
  
  # Generate ROC curve
  roc_curve <- roc(data[[decision_col]], probs)
  
  # Find optimal threshold using Youden's Index
  coords_optimal <- coords(roc_curve, "best", best.method = "youden")
  optimal_threshold <- coords_optimal$threshold[1]
  
  # Create classifications based on optimal threshold
  predicted_class <- ifelse(probs >= optimal_threshold, 1, 0)
  actual_class <- data[[decision_col]]
  
  # Create classification categories
  classification_type <- case_when(
    actual_class == 1 & predicted_class == 1 ~ "True Positive",
    actual_class == 0 & predicted_class == 0 ~ "True Negative", 
    actual_class == 1 & predicted_class == 0 ~ "False Negative",
    actual_class == 0 & predicted_class == 1 ~ "False Positive"
  )
  
  # Create prediction data for smooth curve (on original ICER scale)
  icer_range <- range(data[[icer_col]], na.rm = TRUE)
  pred_data <- data.frame(
    icer_original = seq(icer_range[1], icer_range[2], length.out = 10000)
  )
  names(pred_data)[1] <- icer_col
  
  # Add predictions with confidence intervals
  pred_data$fit <- predict(model, newdata = pred_data, type = "response")
  pred_data$se <- predict(model, newdata = pred_data, type = "link", se.fit = TRUE)$se.fit
  pred_data$lower <- plogis(predict(model, newdata = pred_data, type = "link") - 1.96 * pred_data$se)
  pred_data$upper <- plogis(predict(model, newdata = pred_data, type = "link") + 1.96 * pred_data$se)
  
  
  # Prepare data for plotting
  plot_data <- data.frame(
    icer = data[[icer_col]],
    predicted_prob = probs,
    classification = classification_type
  )
  
  # Logistic regression plot with classification colors and shapes
  colors <- c("True Positive" = "#004D40", 
              "True Negative" = "#D81B60",
              "False Positive" = "#289aff",
              "False Negative" = "#FFC107")
  
  # Define shapes for each classification type - all distinct
  shapes <- c("True Positive" = 16,    # filled circle
              "True Negative" = 15,    # filled square
              "False Positive" = 1,    # filled diamond
              "False Negative" = 0)    # open circle
  
  # Reorder the classification factor to put "False Negative" last (on top)
  plot_data$classification <- factor(plot_data$classification, 
                                     levels = c("True Positive", "True Negative", "False Positive", "False Negative"))
  
  # Sort the data so False Negative rows come last (plotted on top)
  plot_data <- plot_data[order(plot_data$classification), ]
  
  
  logistic_plot <- ggplot() +
    # Add fitted curve
    geom_line(data = pred_data, 
              aes(x = .data[[icer_col]], y = fit),
              color = "deepskyblue4", linewidth = 1) +
    # Add data points with classification colors and shapes
    geom_point(data = plot_data, 
               aes(x = icer, y = predicted_prob, color = classification, shape = classification),
               size = 3.8) +
    # Add threshold line
    geom_hline(yintercept = optimal_threshold, 
               linetype = "dashed", color = "darkgrey", linewidth = 1) +
    # Add threshold text
    annotate("text",
             x = max(data[[icer_col]]) * 0.5,
             y = ifelse(optimal_threshold + 0.05 > 1, optimal_threshold - 0.05, optimal_threshold + 0.05),
             label = paste("Cut-off =", round(optimal_threshold, 3)),
             hjust = 1, size = 3.5) +
    scale_color_manual(values = colors, name = "") +
    scale_shape_manual(values = shapes, name = "") +
    #scale_x_log10(limits = c(min_x, max_x),labels = scales::comma_format()) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      x = "ICER (per €10,000) - Log scale",
      y = "Predicted Probability"
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",)
  
  # Calculate ICER threshold value
  diff_func <- function(icer_value) {
    new_data <- data.frame(x = icer_value)
    names(new_data)[1] <- icer_col
    predicted_prob <- predict(model, newdata = new_data, type = "response")
    return(predicted_prob - optimal_threshold)
  }
  
  
  threshold_value <- uniroot(diff_func,
                             interval = range(data[[icer_col]], na.rm = TRUE))$root
  # find_threshold_precise <- find_threshold_precise(model, data, icer_col, optimal_threshold, precision = 0.0001)
  # threshold_value <- find_threshold_precise$threshold_value
  
  # Format threshold value for readability
  str_TS <- format(round(threshold_value * 10000, 0), big.mark = ",")
  
  # Create summary statistics
  confusion_matrix <- table(Actual = actual_class, Predicted = predicted_class)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Return results
  return(list(
    logistic_plot = logistic_plot,
    optimal_threshold = optimal_threshold,
    threshold_value = threshold_value,
    formatted_TS = str_TS,
    predictions = probs,
    classifications = classification_type,
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    pred_data = pred_data,
    roc_curve = roc_curve
  ))
}


#Updated function to handle grouping variable
gg_outlier_bin_grouped <- function(x,
                                   var_name,
                                   group_var,
                                   cut_off_floor,
                                   cut_off_ceiling,
                                   col = "black",
                                   fill_regular_0 = "red",
                                   fill_regular_1 = "green", 
                                   fill_outliers = "grey",
                                   binwidth = NULL) {
  
  # Filter regular data (between cutoffs)
  x_regular <- x
  if (!is.na(cut_off_ceiling)) {
    x_regular <- x_regular %>% filter(!!sym(var_name) < cut_off_ceiling)
  }
  if (!is.na(cut_off_floor)) {
    x_regular <- x_regular %>% filter(!!sym(var_name) > cut_off_floor)
  }
  
  #Create base plot with grouping
  plot_obj <- ggplot(x_regular, aes(x = !!sym(var_name), fill = !!sym(group_var))) +
    geom_histogram(col = col, binwidth = binwidth, position = "dodge") +
    scale_fill_manual(values = c("0" = fill_regular_0, "1" = fill_regular_1),
                      name = "Decision")
  
  #Handle ceiling outliers
  if (!is.na(cut_off_ceiling)) {
    x_to_roll_ceiling <- x %>% 
      filter(!!sym(var_name) >= cut_off_ceiling) %>%
      mutate(!!var_name := cut_off_ceiling)
    
    if (nrow(x_to_roll_ceiling) > 0) {
      plot_obj <- plot_obj +
        geom_histogram(data = x_to_roll_ceiling, 
                       aes(x = !!sym(var_name)),
                       fill = fill_outliers, 
                       col = col,
                       binwidth = binwidth,
                       inherit.aes = FALSE)
    }
  }
  
  #Handle floor outliers
  if (!is.na(cut_off_floor)) {
    x_to_roll_floor <- x %>% 
      filter(!!sym(var_name) <= cut_off_floor) %>%
      mutate(!!var_name := cut_off_floor)
    
    if (nrow(x_to_roll_floor) > 0) {
      plot_obj <- plot_obj +
        geom_histogram(data = x_to_roll_floor, 
                       aes(x = !!sym(var_name)),
                       fill = fill_outliers, 
                       col = col,
                       binwidth = binwidth,
                       inherit.aes = FALSE)
    }
  }
  
  return(plot_obj)
}