# ESPER_comparison.R

### COMPARE OBSERVED TA, TC IN MERGED BOTTLE DATA TO ESPER PREDICTIONS 

library(tidyverse)
library(ggforce)
library(scales)
library(ModelMetrics)

### READ IN DATA ###
source("scripts/ESPER_analysis/ESPER_out_proc.R")
esper_bottle_combined <- esper_out_proc()

### COMPARE PREDICTIONS AGAINST OBSERVATIONS ###
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
    caption = "ESPER calculations performed using temperature and salinity variables"
  )
ggsave("images/ESPER_comparison/DIC_lim_pred_v_obs.png")

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
    caption = "ESPER calculations performed using all input variables"
  )
ggsave("images/ESPER_comparison/DIC_all_pred_v_obs.png")

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
    caption = "ESPER calculations performed using temperature and salinity variables"
  )
ggsave("images/ESPER_comparison/TA_lim_pred_v_obs.png")

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
    caption = "ESPER calculations performed using all input variables"
  )
ggsave("images/ESPER_comparison/TA_all_pred_v_obs.png")


### INVESTIGATE ANOMALOUS PREDICTIONS ###
esper_bottle_combined %>%
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

esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/anom_boxplot.png")

esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/anom_sal_boxplot.png")

### COMPARE ABSOLUTE RESIDUALS AGAINST OBSERVED ###
esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/TA_lim_res_v_obs.png")

esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/TA_all_res_v_obs.png")

esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/DIC_lim_res_v_obs.png")

esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/DIC_all_res_v_obs.png")

### COMPARE ABSOLUTE RESIDUALS AGAINST ESPER INPUT VARIABLES
esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/TA_all_res_v_input.png")

esper_bottle_combined %>%
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
ggsave("images/ESPER_comparison/DIC_all_res_v_input.png")

### METRICS ###