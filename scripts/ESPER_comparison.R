# ESPER_comparison.R

### COMPARE OBSERVED TA, TC IN MERGED BOTTLE DATA TO ESPER PREDICTIONS 

library(tidyverse)
library(ggforce)
library(scales)
library(ModelMetrics)

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

esper_output$qty_eqn %>% unique()

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

### FITTING ###

# fit TA_lim residuals
TA_lim_fit <- esper_bottle_combined %>%
  mutate(
    Date_Dec = date_decimal(Date.cc)
  ) %>%
  filter(
    Depth <= 20
  ) %>%
  lm(
    formula = TA_lim_res ~ Date_Dec
  )

summary(TA_lim_fit)

# fit TA_all residuals
TA_all_fit <- esper_bottle_combined %>%
  mutate(
    Date_Dec = date_decimal(Date.cc)
  ) %>%
  filter(
    Depth <= 20
  ) %>%
  lm(
    formula = TA_all_res ~ Date_Dec
  )

summary(TA_all_fit)

# fit DIC_lim residuals
DIC_lim_fit <- esper_bottle_combined %>%
  mutate(
    Date_Dec = date_decimal(Date.cc)
  ) %>%
  filter(
    Depth <= 20
  ) %>%
  lm(
    formula = DIC_lim_res ~ Date_Dec
  )

summary(DIC_lim_fit)

# fit DIC_all residuals
DIC_all_fit <- esper_bottle_combined %>%
  mutate(
    Date_Dec = date_decimal(Date.cc)
  ) %>%
  filter(
    Depth <= 20
  ) %>%
  lm(
    formula = DIC_all_res ~ Date_Dec
  )

summary(DIC_all_fit)

### PLOTTING ###
esper_bottle_combined %>%
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
    caption = "From ESPER using temperature and salinity variables"
  )

esper_bottle_combined %>%
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
    caption = "From ESPER using all input variables"
  )

esper_bottle_combined %>%
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
    caption = "From ESPER using temperature and salinity variables"
  )

esper_bottle_combined %>%
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
    caption = "From ESPER using all input variables"
  )

### Metrics ###
esper_bottle_combined <- esper_bottle_combined %>%
  mutate(
    TA_lim_absolute_res = abs(TA - TA_lim),
    TA_all_absolute_res = abs(TA - TA_all),
    DIC_lim_absolute_res = abs(DIC - DIC_lim),
    DIC_all_absolute_res = abs(DIC - DIC_all)
  )


TA_lim_rmse <- sqrt(mean((esper_bottle_combined$TA - esper_bottle_combined$TA_lim)^2 , na.rm = TRUE))
TA_all_rmse <- sqrt(mean((esper_bottle_combined$TA - esper_bottle_combined$TA_all)^2 , na.rm = TRUE))
DIC_lim_rmse <- sqrt(mean((esper_bottle_combined$DIC - esper_bottle_combined$DIC_lim)^2 , na.rm = TRUE))
DIC_all_rmse <- sqrt(mean((esper_bottle_combined$DIC - esper_bottle_combined$DIC_all)^2 , na.rm = TRUE))

