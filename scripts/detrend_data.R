# detrend.R

### FUNCTION TO SEASONALLY DETREND OCEANOGRAPHIC DATA

library(tidyverse)

sea_dtd_data <- function(qty, df, date_col) {
  ### Seasonally detrend selected columns in a dataframe based on Sutton, A. J. et al. (2022)
  ### qty: vector of column names of variables to be detrended
  ### df: dataframe with observations to be detrended
  ### date_col: date column to be used for fitting
  
  # Check if date_col is in decimal format and convert if not
  if (!is.double(date_col)) {
    df <- df %>%
      mutate(
        Date_Dec = decimal_date(get(date_col))
      )
  }
  
  # Detrend data for each desired quantity
  for (i in qty) {
    # Extract overall linear trend
    lin_trend <- lm(get(i) ~ Date_Dec, data = df, na.action = na.exclude)
    
    # Remove overall linear trend
    df <- df %>%
      mutate(
        lin_dtd_obs = as.vector(residuals(lin_trend))
      )
    
    # Bin observations into a three month sliding scale
    df_minus <- df %>% 
      mutate(
        bin_month = ifelse((Month_UTC - 1) == 0, 12, Month_UTC - 1)
      )
    df_0 <- df %>%
      mutate(
        bin_month = Month_UTC
      )
    df_plus <- df %>%
      mutate(
        bin_month = ifelse((Month_UTC + 1) == 13, 1, Month_UTC + 1)
      )
    df_binned <- bind_rows(df_minus, df_0, df_plus)
    
    # Compute monthly means of each 3-month bin
    monthly_means_df <- df_binned %>%
      group_by(
        Year_UTC, bin_month
      ) %>%
      summarize(
        monthly_mean = mean(lin_dtd_obs, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Compute anomaly for each bin
    monthly_means_df <- monthly_means_df %>%
      mutate(
        anomaly = monthly_mean - mean(monthly_mean, na.rm = TRUE)
      )
    
    # Subtract anomalies from observations
    df <- df %>% 
      left_join(
        monthly_means_df,
        by = join_by(Year_UTC, Month_UTC == bin_month)
      ) %>%
      mutate(
        i_dtd = get(i) - anomaly
      ) %>%
      rename_with(
        .cols = i_dtd,
        ~ paste0(i, "_dtd")
      )
    
    # Remove extra columns that are no longer needed
    df <- df %>% 
      select(
        -c(lin_dtd_obs,monthly_mean,anomaly)
      )
  }
  
  # Return dataframe as output
  return(df)
}