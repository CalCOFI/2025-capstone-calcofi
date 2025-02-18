library(tidyverse)
library(ggforce)
library(scales)
library(ModelMetrics)

### Correct Anomalous Salnty Values ###
merged_bottle_data <- read_csv(here::here("data/merged_bottle_data.csv"))

merged_bottle_anom_fix <- merged_bottle_data |> 
  mutate(anom = case_when(
    Salnty < 30 ~ TRUE,
    TRUE ~ FALSE)) |> 
  mutate(Salnty = case_when(
    anom ~ Salnty + 10,
    TRUE ~ Salnty
  ))

write_csv(merged_bottle_anom_fix, file = here::here("data/merged_bottle_anom_fix.csv"))

### Combine Data with ESPER anom fixes ###

esper_out_anom_proc <- function() {
  ### READ IN DATA ###
  
  # read in combined bottle dataset
  merged_bottle_data <- read_csv("data/merged_bottle_anom_fix.csv")
  
  # read in ESPER output
  esper_estimates_lim <- read_csv("data/ESPER_output/ESPER_estimates_anom_fix_lim.csv")
  esper_estimates_all <- read_csv("data/ESPER_output/ESPER_estimates_anom_fix_all.csv")
  esper_uncertainties_lim <- read_csv("data/ESPER_output/ESPER_uncertainties_anom_fix_lim.csv")
  esper_uncertainties_all <- read_csv("data/ESPER_output/ESPER_uncertainties_anom_fix_all.csv")
  
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
esper_bottle_anom <- esper_out_anom_proc()

### COMPARE PREDICTIONS AGAINST OBSERVATIONS ###
esper_bottle_anom %>%
  ggplot(
    aes(
      x = DIC,
      y = DIC_lim,
      color = Depth
    )
  ) +
  geom_point(
    na.rm = TRUE
  ) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  scale_color_gradient(
    trans = trans_reverser("pseudo_log"),
    breaks = c(1,10,100,1000)
  ) +
  theme_minimal() +
  labs(
    x = "Observed DIC",
    y = "Predicted DIC",
    caption = "ESPER calculations performed using temperature and salinity variables, anomaly corrected"
  )

esper_bottle_anom %>%
  ggplot(
    aes(
      x = DIC,
      y = DIC_all,
      color = Depth
    )
  ) +
  geom_point(
    na.rm = TRUE
  ) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  scale_color_gradient(
    trans = trans_reverser("pseudo_log"),
    breaks = c(1,10,100,1000)
  ) +
  theme_minimal() +
  labs(
    x = "Observed DIC",
    y = "Predicted DIC",
    caption = "ESPER calculations performed using all input variables, anomaly corrected"
  )

esper_bottle_anom %>%
  ggplot(
    aes(
      x = TA,
      y = TA_lim,
      color = Depth
    )
  ) +
  geom_point(
    na.rm = TRUE
  ) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  scale_color_gradient(
    trans = trans_reverser("pseudo_log"),
    breaks = c(1,10,100,1000)
  ) +
  theme_minimal() +
  labs(
    x = "Observed TA",
    y = "Predicted TA",
    caption = "ESPER calculations performed using temperature and salinity variables, anomaly corrected"
  )

esper_bottle_anom %>%
  ggplot(
    aes(
      x = TA,
      y = TA_all,
      color = Depth
    )
  ) +
  geom_point(
    na.rm = TRUE
  ) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  scale_color_gradient(
    trans = trans_reverser("pseudo_log"),
    breaks = c(1,10,100,1000)
  ) +
  theme_minimal() +
  labs(
    x = "Observed TA",
    y = "Predicted TA",
    caption = "ESPER calculations performed using all input variables, anomaly corrected"
  )



### INVESTIGATE ANOMALOUS PREDICTIONS ###
esper_bottle_anom %>%
  filter(
    (TA > 2200) & (TA < 2275)
  ) %>%
  filter(
    !is.na(TA_all)
  ) %>%
  mutate(
    anom = ifelse(TA_all < 2000, 1, 0)
  ) %>%
  group_by(
    anom
  ) %>%
  summarize(
    Stations = n_distinct(Station_ID),
    Temp = mean(T_degC, na.rm = TRUE),
    Sal = mean(Salnty, na.rm = TRUE),
    PO4uM = mean(PO4uM, na.rm = TRUE),
    NO3uM = mean(NO3uM, na.rm = TRUE),
    SiO3uM = mean(SiO3uM, na.rm = TRUE),
    `Oxy_µmol/Kg` = mean(`Oxy_µmol/Kg`, na.rm = TRUE)
  )

esper_bottle_anom %>%
  filter(
    (TA > 2200) & (TA < 2275)
  ) %>%
  filter(
    !is.na(TA_all)
  ) %>%
  mutate(
    anom = ifelse(TA_all < 2000, 1, 0)
  ) %>%
  pivot_longer(
    cols = c(T_degC, Salnty, PO4uM, NO3uM, SiO3uM, `Oxy_µmol/Kg`),
    names_to = "input_var",
    values_to = "input_var_val"
  ) %>%
  ggplot(
    aes(
      x = factor(anom),
      y = input_var_val,
      fill = factor(anom)
    )
  ) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(
    vars(input_var),
    scales = "free"
  ) + 
  theme_minimal() +
  labs(
    x = NULL,
    y = "Value",
  ) +
  scale_x_discrete(
    labels = c("Normal", "Anomalous")
  ) +
  guides(
    fill = "none"
  )

esper_bottle_anom %>%
  filter(
    !is.na(TA_all)
  ) %>%
  mutate(
    anom = ifelse(TA_all < 2000, 1, 0)
  ) %>%
  ggplot(
    aes(
      x = factor(anom),
      y = Salnty,
      fill = factor(anom)
    )
  ) +
  geom_boxplot(na.rm = TRUE) +
  theme_minimal() +
  labs(
    x = NULL
  ) +
  scale_x_discrete(
    labels = c("Normal Predictions", "Anomalous Predictions")
  ) +
  guides(
    fill = "none"
  )

### COMPARE ABSOLUTE RESIDUALS AGAINST OBSERVED ###
esper_bottle_anom %>%
  ggplot(
    aes(
      x = TA,
      y = abs(TA_lim_res)
    )
  ) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(
    caption = "ESPER calculations performed using temperature and salinity input variables",
    y = "Absolute Residuals",
    x = "Observed TA"
  )

esper_bottle_anom %>%
  ggplot(
    aes(
      x = TA,
      y = abs(TA_all_res)
    )
  ) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(
    caption = "ESPER calculations performed using all input variables",
    y = "Absolute Residuals",
    x = "Observed TA"
  )

esper_bottle_anom %>%
  ggplot(
    aes(
      x = DIC,
      y = abs(DIC_lim_res)
    )
  ) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(
    caption = "ESPER calculations performed using temperature and salinity input variables",
    y = "Absolute Residuals",
    x = "Observed DIC"
  )

esper_bottle_anom %>%
  ggplot(
    aes(
      x = DIC,
      y = abs(DIC_all_res)
    )
  ) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(
    caption = "ESPER calculations performed using all input variables",
    y = "Absolute Residuals",
    x = "Observed DIC"
  )

### COMPARE ABSOLUTE RESIDUALS AGAINST ESPER INPUT VARIABLES
esper_bottle_anom %>%
  filter(
    abs(TA_lim_res) < 200
  ) %>%
  pivot_longer(
    cols = c("Salnty","T_degC","PO4uM","NO3uM","SiO3uM","Oxy_µmol/Kg"),
    names_to = "input_var",
    values_to = "input_var_val"
  ) %>%
  ggplot(
    aes(
      x = input_var_val,
      y = abs(TA_all_res)
    )
  ) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  facet_wrap(
    vars(input_var),
    scales = "free"
  ) +
  labs(
    caption = "ESPER calculations performed using all input variables",
    y = "Absolute TA Residuals",
    x = NULL
  )


esper_bottle_anom %>%
  filter(
    abs(TA_lim_res) < 200
  ) %>%
  pivot_longer(
    cols = c("Salnty","T_degC","PO4uM","NO3uM","SiO3uM","Oxy_µmol/Kg"),
    names_to = "input_var",
    values_to = "input_var_val"
  ) %>%
  ggplot(
    aes(
      x = input_var_val,
      y = abs(DIC_all_res)
    )
  ) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  facet_wrap(
    vars(input_var),
    scales = "free"
  ) +
  labs(
    caption = "ESPER calculations performed using all input variables",
    y = "Absolute DIC Residuals",
    x = NULL
  )

### Comparison ###

source("scripts/ESPER_analysis/ESPER_out_proc.R")
esper_bottle_combined <- esper_out_proc()

esper_bottle_combined <- esper_bottle_combined |> 
  mutate(anom = case_when(
    Salnty < 30 ~ TRUE,
    TRUE ~ FALSE
  ))

esper_bottle_combined |> 
  filter(anom) |> 
  select(Salnty, DIC_all_res, TA_all_res) |> 
  mutate(DIC_all_abs_res = abs(DIC_all_res),
         TA_all_abs_res = abs(TA_all_res)) |> 
  select(Salnty, DIC_all_abs_res, TA_all_abs_res) |> 
  kableExtra::kable() |> 
  kableExtra::kable_styling()


esper_bottle_anom |> 
  filter(anom) |> 
  select(Salnty, DIC_all_res, TA_all_res) |> 
  mutate(DIC_all_abs_res = abs(DIC_all_res),
         TA_all_abs_res = abs(TA_all_res)) |> 
  select(Salnty, DIC_all_abs_res, TA_all_abs_res) |> 
  kableExtra::kable() |> 
  kableExtra::kable_styling()
