# anomaly_analysis.R

library(tidyverse)

# READ IN DATA ------------------------------------------------------------
source("scripts/ESPER_analysis/ESPER_out_proc.R")
esper_bottle_combined <- esper_out_proc() %>%
  mutate(
    ESPER_input = factor(ESPER_input, levels = c("lim", "all"), ordered = TRUE)
  ) %>%
  filter(
    Salnty > 30
  )

models <- c("Mixed", "LIR", "NN")
inputs <- c("lim", "all")
vars <- c("TA", "DIC")


# INVESTIGATE ANOMALOUS PREDICTIONS ---------------------------------------

# calculate input variable means
esper_bottle_combined %>%
  filter(
    ESPER_model == "Mixed"
  ) %>%
  filter(
    ESPER_input == "all"
  ) %>%
  filter(
    (TA > 2200) & (TA < 2275)
  ) %>%
  filter(
    !is.na(TA_est)
  ) %>%
  mutate(
    anom = ifelse(TA_est < 2000, 1, 0)
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

# make boxplot of input variables
esper_bottle_combined %>%
  filter(
    ESPER_model == "Mixed"
  ) %>%
  filter(
    ESPER_input == "all"
  ) %>%
  filter(
    (TA > 2200) & (TA < 2275)
  ) %>%
  filter(
    !is.na(TA_est)
  ) %>%
  mutate(
    anom = ifelse(TA_est < 2000, 1, 0)
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
ggsave("images/ESPER_analysis/anom_boxplot.png")

# create salinity boxplot
esper_bottle_combined %>%
  filter(
    ESPER_model == "Mixed"
  ) %>%
  filter(
    ESPER_input == "lim"
  ) %>%
  filter(
    !is.na(TA_est)
  ) %>%
  mutate(
    anom = ifelse(TA_est < 2000, 1, 0)
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
    x = NULL,
    y = "Salinity"
  ) +
  scale_x_discrete(
    labels = c("Normal Predictions", "Anomalous Predictions")
  ) +
  guides(
    fill = "none"
  )
ggsave("images/ESPER_analysis/anom_sal_boxplot.png")