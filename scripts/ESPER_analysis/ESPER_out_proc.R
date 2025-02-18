# ESPER_out_proc.R

### FUNCTION TO PROCESS ESPER OUTPUT AND MERGE WITH COMBINED BOTTLE DATA

### Computes weighted average ESPER estimates by uncertainties, combines
### result with merged bottle data, and calculates residuals

library(tidyverse)

esper_out_proc <- function() {
  ### READ IN DATA ###
  
  # read in combined bottle dataset
  merged_bottle_data <- read_csv("data/merged_bottle_data.csv")
  
  # read in ESPER output
  esper_estimates_lim <- read_csv("data/ESPER_output/ESPER_estimates_lim.csv")
  esper_estimates_all <- read_csv("data/ESPER_output/ESPER_estimates_all.csv")
  esper_uncertainties_lim <- read_csv("data/ESPER_output/ESPER_uncertainties_lim.csv")
  esper_uncertainties_all <- read_csv("data/ESPER_output/ESPER_uncertainties_all.csv")
  
  ### DATA CLEANING/MANIPULATION ###
  
  # pivot ESPER output to long format for easier manipulation
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