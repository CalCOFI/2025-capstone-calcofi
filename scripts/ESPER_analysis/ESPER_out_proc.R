# ESPER_out_proc.R

### ESPER OUTPUT PROCESSING FUNCTIONS

library(tidyverse)

### Function to average ESPER output weighted by uncertainties
esper_out_avg <- function(est_df, unc_df, suffix = NA) {
  
  # pivot dataframes to long format
  est_df <- est_df %>%
    # add row id column before pivoting
    mutate(
      id = row_number()
    ) %>%
    pivot_longer(
      -id,
      names_to = "qty_eqn",
      values_to = "est"
    ) %>%
    # create quantity and equation columns
    mutate(
      qty = sub("([a-zA-Z]+)_[0-9]+", "\\1", qty_eqn),
      eqn = sub("[a-zA-Z]+_([0-9]+)", "\\1", qty_eqn),
      .keep = "unused",
      .before = "est"
    )
  
  unc_df <- unc_df %>%
    # add row id column before pivoting
    mutate(
      id = row_number()
    ) %>%
    pivot_longer(
      -id,
      names_to = "qty_eqn",
      values_to = "unc"
    ) %>%
    # create quantity and equation columns
    mutate(
      qty = sub("([a-zA-Z]+)_[0-9]+", "\\1", qty_eqn),
      eqn = sub("[a-zA-Z]+_([0-9]+)", "\\1", qty_eqn),
      .keep = "unused",
      .before = "unc"
    )
  
  # combine estimate and uncertainty dataframes
  df <- inner_join(
    est_df,
    unc_df,
    by = join_by(id, qty, eqn)
  )
  
  # check that number of rows matches original dataframes
  nrow(df) == nrow(est_df)
  nrow(df) == nrow(unc_df)
  
  # compute weighted average of estimates
  df <- df %>%
    group_by(
      id, qty
    ) %>%
    summarize(
      est = weighted.mean(est, unc^2, na.rm = TRUE),
      unc = 1/sqrt(sum(1/unc^2, na.rm = TRUE))
    ) %>%
    ungroup()
  
  # pivot to wide format
  df <- df %>%
    pivot_wider(
      id_cols = id,
      names_from = qty,
      values_from = c(est, unc),
      names_glue = paste0("{qty}_{.value}",if(is.na(suffix)) NULL else paste0("_",suffix))
    )
  
  # drop id column
  df <- df %>%
    select(
      -id
    )
  
  # return dataframe of averaged estimates and uncertainties
  return(df)
}

esper_out_proc <- function() {
  ### READ IN DATA ###
  
  # read in combined bottle dataset
  merged_bottle_data <- read_csv("data/merged_bottle_data.csv")
  
  # read in ESPER output
  esper_estimates_lim <- read_csv("data/ESPER_output/ESPER_Mixed_est_lim.csv")
  esper_estimates_all <- read_csv("data/ESPER_output/ESPER_Mixed_est_all.csv")
  esper_uncertainties_lim <- read_csv("data/ESPER_output/ESPER_Mixed_unc_lim.csv")
  esper_uncertainties_all <- read_csv("data/ESPER_output/ESPER_Mixed_unc_all.csv")
  
  ### DATA CLEANING/MANIPULATION ###
  
  # pivot ESPER output to long format
  esper_estimates_lim <- esper_estimates_lim %>%
    # add id column to keep track of output groupings
    mutate(
      id = row_number()
    ) %>%
    pivot_longer(
      cols = -id,
      names_to = "qty_eqn",
      values_to = "est_lim",
    )
  esper_estimates_all <- esper_estimates_all %>%
    # add id column to keep track of output groupings
    mutate(
      id = row_number()
    ) %>%
    pivot_longer(
      cols = -id,
      names_to = "qty_eqn",
      values_to = "est_all"
    )
  esper_uncertainties_lim <- esper_uncertainties_lim %>%
    # add id column to keep track of output groupings
    mutate(
      id = row_number()
    ) %>%
    pivot_longer(
      cols = -id,
      names_to = "qty_eqn",
      values_to = "unc_lim"
    )
  esper_uncertainties_all <- esper_uncertainties_all %>%
    # add id column to keep track of output groupings
    mutate(
      id = row_number()
    ) %>%
    pivot_longer(
      cols = -id,
      names_to = "qty_eqn",
      values_to = "unc_all"
    )
  
  # combine ESPER output into single dataframe
  esper_output <- inner_join(
    esper_estimates_lim,
    esper_estimates_all,
    by = join_by(id, qty_eqn)
  ) %>%
    inner_join(
      esper_uncertainties_lim,
      by = join_by(id, qty_eqn)
    ) %>%
    inner_join(
      esper_uncertainties_all,
      by = join_by(id, qty_eqn)
    )
  
  # combine ESPER output from 16 equations using a weighted average by uncertainty
  esper_output <- esper_output %>%
    mutate(
      qty = sub("([a-zA-Z]+)_[0-9]+", "\\1", qty_eqn),
      .keep = "unused"
    ) %>%
    group_by(
      id, qty
    ) %>%
    summarize(
      est_lim = weighted.mean(est_lim, 1/unc_lim, na.rm = TRUE),
      est_all = weighted.mean(est_all, 1/unc_all, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # pivot ESPER output back to wide format to merge with combined bottle data
  esper_output <- esper_output %>%
    pivot_wider(
      names_from = qty,
      values_from = c(est_lim, est_all),
    ) %>%
    rename_with(
      ~ sub("[a-zA-Z]+_([a-zA-Z]+)_([a-zA-Z]+)", "\\2_\\1", .x)
    ) %>%
    select(
      -id
    )
  
  # combine ESPER output with combined bottle data
  esper_bottle_combined <- bind_cols(
    merged_bottle_data,
    esper_output
  )
  
  # calculate residuals (obs - ESPER)
  esper_bottle_combined <- esper_bottle_combined %>%
    mutate(
      TA_lim_res = TA - TA_lim,
      TA_all_res = TA - TA_all,
      DIC_lim_res = DIC - DIC_lim,
      DIC_all_res = DIC - DIC_all
    )
  
  # return combined ESPER output and bottle data
  return(esper_bottle_combined)
}