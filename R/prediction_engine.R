#=============================================================================
# R/prediction_engine.R
# Ebola 2026 Project - Adaptive Trend Projection Engine (Anchored Local Fit)
#=============================================================================

generate_surveillance_predictions <- function(historical_df, metric_col) {
  # 1. Replicate the exact aggregation pipeline used by the visualization
  timeline_df <- historical_df %>%
    group_by(date) %>%
    summarise(
      total = sum(as.numeric(.data[[metric_col]]), na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    arrange(date)
  
  n_obs <- nrow(timeline_df)
  
  # Fail-safe guard: If data is insufficient, project forward from zero
  if (n_obs < 2) {
    future_dates <- seq(Sys.Date() + 1, by = "day", length.out = 30)
    return(data.frame(date = future_dates, display_val = 0, type = "Prediction"))
  }
  
  # 2. Extract structural anchors
  last_record <- tail(timeline_df, 1)
  last_date <- as.Date(last_record$date)
  last_value <- last_record$total
  
  # 3. Calculate sequence intervals (detect if data is daily, weekly, or monthly)
  date_steps <- diff(timeline_df$date)
  median_step <- as.numeric(median(date_steps, na.rm = TRUE))
  if (is.na(median_step) || median_step == 0) median_step <- 1
  
  # 4. Localized Slope Estimation (Ordinary Least Squares on the observed timeline)
  y <- timeline_df$total
  x <- 1:n_obs
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  denom <- sum((x - x_mean)^2)
  if (denom != 0) {
    slope <- sum((x - x_mean) * (y - y_mean)) / denom
  } else {
    slope <- 0
  }
  
  # 5. Generate continuous future coordinates (30 steps forward matching data cadence)
  future_dates <- seq(last_date + median_step, by = median_step, length.out = 30)
  predictions <- numeric(30)
  
  for (i in 1:30) {
    # Project forward adding momentum relative to the true final baseline anchor
    pred_val <- last_value + (slope * i)
    predictions[i] <- max(0, round(pred_val))
  }
  
  data.frame(
    date = future_dates,
    display_val = predictions,
    type = "Prediction"
  )
}