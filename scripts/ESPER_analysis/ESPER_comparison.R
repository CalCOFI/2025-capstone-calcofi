# ESPER_comparison.R

### COMPARE OBSERVED TA, TC IN MERGED BOTTLE DATA TO ESPER PREDICTIONS 

library(tidyverse)
library(ggforce)
library(scales)
library(ModelMetrics)
library(gt)
library(cowplot)

### READ IN DATA ###
source("scripts/ESPER_analysis/ESPER_out_proc.R")
esper_bottle_combined <- esper_out_proc()

models <- c("Mixed", "LIR", "NN")
inputs <- c("lim", "all")
vars <- c("TA", "DIC")

### COMPARE PREDICTIONS AGAINST OBSERVATIONS ###

# DIC predictions
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = DIC,
      y = DIC_est,
      col = Depth
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.3
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  scale_color_gradient(
    trans = trans_reverser("pseudo_log"),
    breaks = c(1,10,100,1000)
  ) +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  theme_minimal() +
  labs(
    x = "Observed DIC",
    y = "Predicted DIC",
  )
ggsave("images/ESPER_comparison/DIC_pred_vs_obs.png", bg = "white")

# TA predictions
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = TA,
      y = TA_est,
      col = Depth
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.3
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  scale_color_gradient(
    trans = trans_reverser("pseudo_log"),
    breaks = c(1,10,100,1000)
  ) +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  theme_minimal() +
  labs(
    x = "Observed TA",
    y = "Predicted TA",
  )
ggsave("images/ESPER_comparison/TA_pred_vs_obs.png", bg = "white")

### INVESTIGATE ANOMALOUS PREDICTIONS ###

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
ggsave("images/ESPER_comparison/anom_boxplot.png")

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
ggsave("images/ESPER_comparison/anom_sal_boxplot.png")

### COMPARE ABSOLUTE RESIDUALS AGAINST OBSERVED ###

# DIC residuals
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = DIC,
      y = DIC_res
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.25
  ) +
  theme_minimal() +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) + 
  labs(
    y = "DIC Residuals (Observed - ESPER)",
    x = "Observed DIC"
  )
ggsave("images/ESPER_comparison/DIC_res_v_obs.png", bg = "white")

# TA residuals
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = TA,
      y = TA_res
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.25
  ) +
  theme_minimal() +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) + 
  labs(
    y = "TA Residuals (Observed - ESPER)",
    x = "Observed TA"
  )
ggsave("images/ESPER_comparison/TA_res_v_obs.png", bg = "white")

### COMPARE ABSOLUTE RESIDUALS AGAINST ESPER INPUT VARIABLES

# DIC residuals
DIC_input_plots <- NULL
for (i in 1:6) {
  plot <- esper_bottle_combined %>%
    filter(
      Salnty > 30
    ) %>%
    filter(
      (ESPER_model == models[i%%3+1]) & (ESPER_input == inputs[i%%2+1])
    ) %>%
    pivot_longer(
      cols = c("Salnty","T_degC","PO4uM","NO3uM","SiO3uM","Oxy_µmol/Kg"),
      names_to = "input_var",
      values_to = "input_var_val"
    ) %>%
    ggplot(
      aes(
        x = input_var_val,
        y = DIC_res,
        col = input_var
      )
    ) +
    geom_point(
      na.rm = TRUE,
      alpha = 0.2
    ) +
    theme_minimal() +
    facet_wrap(
      vars(input_var),
      scales = "free"
    ) +
    labs(
      x = NULL,
      y = "DIC Residuals",
      title = paste("ESPER", models[i%%3+1], toupper(inputs[i%%2+1]))
    ) +
    guides(
      col = "none"
    )
  DIC_input_plots[[i]] <- plot
}
plot_grid(DIC_input_plots[[1]], DIC_input_plots[[5]], DIC_input_plots[[3]], DIC_input_plots[[4]], DIC_input_plots[[2]], DIC_input_plots[[6]], nrow = 2)
ggsave("images/ESPER_comparison/DIC_res_v_input.png", scale = 2, bg = "white")

# TA residuals
TA_input_plots <- NULL
for (i in 1:6) {
  plot <- esper_bottle_combined %>%
    filter(
      Salnty > 30
    ) %>%
    filter(
      (ESPER_model == models[i%%3+1]) & (ESPER_input == inputs[i%%2+1])
    ) %>%
    pivot_longer(
      cols = c("Salnty","T_degC","PO4uM","NO3uM","SiO3uM","Oxy_µmol/Kg"),
      names_to = "input_var",
      values_to = "input_var_val"
    ) %>%
    ggplot(
      aes(
        x = input_var_val,
        y = TA_res,
        col = input_var
      )
    ) +
    geom_point(
      na.rm = TRUE,
      alpha = 0.2
    ) +
    theme_minimal() +
    facet_wrap(
      vars(input_var),
      scales = "free"
    ) +
    labs(
      x = NULL,
      y = "TA Residuals",
      title = paste("ESPER", models[i%%3+1], toupper(inputs[i%%2+1]))
    ) +
    guides(
      col = "none"
    )
  TA_input_plots[[i]] <- plot
}
plot_grid(TA_input_plots[[1]], TA_input_plots[[5]], TA_input_plots[[3]], TA_input_plots[[4]], TA_input_plots[[2]], TA_input_plots[[6]], nrow = 2)
ggsave("images/ESPER_comparison/TA_res_v_input.png", scale = 2, bg = "white")


### COMPARE RESIDUALS AGAINST DEPTH, TIME

# DIC residuals against depth
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = Depth,
      y = DIC_res
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.3
  ) +
  scale_x_continuous(
    transform = "pseudo_log",
    breaks = c(1,10,100,1000)
  ) +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  theme_minimal() +
  labs(
    x = "Depth (m)",
    y = "DIC Residuals",
  )
ggsave("images/ESPER_comparison/DIC_res_v_depth.png", bg = "white")

# TA residuals against depth
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = Depth,
      y = TA_res
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.3
  ) +
  scale_x_continuous(
    transform = "pseudo_log",
    breaks = c(1,10,100,1000)
  ) +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  theme_minimal() +
  labs(
    x = "Depth (m)",
    y = "TA Residuals",
  )
ggsave("images/ESPER_comparison/TA_res_v_depth.png", bg = "white")

# DIC residuals against date
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = Date.cc,
      y = DIC_res
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.3
  ) +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "DIC Residuals",
  )
ggsave("images/ESPER_comparison/DIC_res_v_date.png", bg = "white")

# TA residuals against date
esper_bottle_combined %>%
  filter(
    Salnty > 30
  ) %>%
  ggplot(
    aes(
      x = Date.cc,
      y = TA_res
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.3
  ) +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "TA Residuals",
  )
ggsave("images/ESPER_comparison/TA_res_v_date.png", bg = "white")

### METRICS ###

# calculate relative residuals
esper_bottle_combined <- esper_bottle_combined %>%
  mutate(
    TA_rel = TA_res/TA,
    DIC_rel = DIC_res/DIC
  )

# calcualte standard deviation of observed values
TA_sd <- esper_bottle_combined$TA %>% sd(na.rm = TRUE)
DIC_sd <- esper_bottle_combined$DIC %>% sd(na.rm = TRUE)

# generate table of RMSE values by model and input
esper_bottle_combined %>%
  group_by(
    ESPER_model, ESPER_input
  ) %>%
  summarize(
    TA_rmse = sqrt(sum(TA_res^2/sum(!is.na(TA_res)), na.rm = TRUE)),
    DIC_rmse = sqrt(sum(DIC_res^2/sum(!is.na(DIC_res)), na.rm = TRUE)),
  ) %>%
  gt(
    row_group_as_column = TRUE
  ) %>%
  tab_header(
    title = "ESPERs RMSE"
  ) %>%
  tab_stubhead(
    label = "Model"
  ) %>%
  cols_label(
    ESPER_input = "Input",
    TA_rmse = "TA",
    DIC_rmse = "DIC"
  ) %>%
  tab_footnote(
    footnote = "lim refers to ESPER calculations performed using only temperature and salinity as predictors; all refers to calculations using all six input variables",
    locations = cells_column_labels(ESPER_input)
  ) %>%
  tab_footnote(
    footnote = paste("SD =", signif(TA_sd,7)),
    locations = cells_column_labels(TA_rmse)
  ) %>%
  tab_footnote(
    footnote = paste("SD =", signif(DIC_sd, 8)),
    locations = cells_column_labels(DIC_rmse)
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  gtsave("images/ESPER_comparison/rmse_table.png")


# RMSE by depths
esper_bottle_combined %>%
  mutate(
    depth_bins = case_when(
      (Depth >= 0) & (Depth <= 50) ~ "0 to 50m",
      (Depth > 50) & (Depth <= 200) ~ "51 to 200m",
      (Depth > 201) & (Depth <= 400) ~ "201 to 400m",
      (Depth > 401) & (Depth <= 1000) ~ "401 to 1000m",
      (Depth > 1001) ~ ">1001m"
    ) %>% factor(levels = c("0 to 50m", "51 to 200m", "201 to 400m", 
                            ("401 to 1000m"), (">1001m")),
                 ordered = TRUE)
  ) %>%
  group_by(
    ESPER_model, ESPER_input, depth_bins
  ) %>%
  summarize(
    n = n(),
    TA_rmse = sqrt(sum(TA_res^2/sum(!is.na(TA_res)), na.rm = TRUE)),
    TA_sd = sd(TA, na.rm = TRUE),
    DIC_rmse = sqrt(sum(DIC_res^2/sum(!is.na(DIC_res)), na.rm = TRUE)),
    DIC_sd = sd(DIC, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    TA_rmse_sd = TA_rmse/TA_sd,
    DIC_rmse_sd = DIC_rmse/DIC_sd
  ) %>%
  group_by(
    ESPER_model
  ) %>%
  gt(
    row_group_as_column = TRUE
  ) %>%
  tab_header(
    title = "ESPERs RMSE by Depth"
  ) %>%
  tab_stubhead(
    label = "Model"
  ) %>%
  cols_label(
    ESPER_input = "Input",
    depth_bins = "Depth",
    n = "N",
    TA_rmse = "TA RMSE",
    TA_sd = "TA SD",
    TA_rmse_sd = "TA RMSE/SD",
    DIC_rmse = "DIC RMSE",
    DIC_sd = "DIC SD",
    DIC_rmse_sd = "DIC RMSE/SD"
  ) %>%
  tab_footnote(
    footnote = "lim refers to ESPER calculations performed using only temperature and salinity as predictors; all refers to calculations using all six input variables",
    locations = cells_column_labels(ESPER_input)
  ) %>%
  opt_stylize(
    style = 3
  )

# RMSE by year
esper_bottle_combined %>%
  group_by(
    ESPER_model, ESPER_input, Year_UTC
  ) %>%
  summarize(
    TA_rmse = sqrt(sum(TA_res^2/sum(!is.na(TA_res)), na.rm = TRUE)),
    TA_sd = sd(TA, na.rm = TRUE),
    DIC_rmse = sqrt(sum(DIC_res^2/sum(!is.na(DIC_res)), na.rm = TRUE)),
    DIC_sd = sd(DIC, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    TA_rmse_sd = TA_rmse/TA_sd,
    DIC_rmse_sd = DIC_rmse/DIC_sd,
    .keep = "unused"
  ) %>% 
  add_row(
    ESPER_model = rep(c("LIR", "Mixed", "NN"), each = 12),
    ESPER_input = rep(c("all","lim"), each = 6, length.out = 36),
    Year_UTC = rep(2002:2007,6), 
    DIC_rmse_sd = rep(NA, 36),
    TA_rmse_sd = rep(NA, 36)
  ) %>% View()
  pivot_longer(
    cols = c(TA_rmse_sd, DIC_rmse_sd),
    names_to = "qty",
    values_to = "rmse_sd"
  ) %>%
  ggplot(
    aes(
      x = Year_UTC,
    )
  ) +
  geom_line(
    aes(
      y = rmse_sd,
      group = qty,
      col = qty
    ) 
  ) +
  theme_minimal() + 
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  scale_color_discrete(
    labels = c("DIC", "TA"),
    type = c("blue", "red")
  ) +
  labs(
    y = "RMSE/SD",
    x = "Year",
    col = "Quantity"
  )
ggsave("images/ESPER_comparison/rmse_by_year.png")

# generate table of TA residuals summary statistics 
esper_bottle_combined %>%
  group_by(
    ESPER_model, ESPER_input
  ) %>%
  reframe(
    abs = as.list(summary(TA_res)),
    rel = as.list(summary(TA_rel))
  ) %>%
  unnest(
    c(abs, rel)
  ) %>%
  `[`(-(1:12*7),) %>%
  mutate(
    stat = rep(names(summary(abs)), 6)
  ) %>%
  pivot_longer(
    cols = c(abs, rel),
    names_to = "res_type",
    values_to = "res_val"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = "res_val"
  ) %>%
  group_by(
    ESPER_model
  ) %>%
  gt(
    row_group_as_column = TRUE
  ) %>%
  tab_header(
    title = "ESPERs TA Residuals"
  ) %>%
  tab_stubhead(
    label = "Model"
  ) %>%
  cols_label(
    ESPER_input = "Input",
    res_type = "Res. Type",
  ) %>%
  tab_footnote(
    footnote = "lim refers to ESPER calculations performed using only temperature and salinity as predictors; all refers to calculations using all six input variables",
    locations = cells_column_labels(ESPER_input)
  ) %>%
  tab_footnote(
    footnote = "abs = absolute residuals; rel = relative residuals",
    locations = cells_column_labels(res_type)
  ) %>%
  fmt_number(
    decimals = 4
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  gtsave("images/ESPER_comparison/TA_res_table.png")

# generate table of DIC residuals summary statistics
esper_bottle_combined %>%
  group_by(
    ESPER_model, ESPER_input
  ) %>%
  reframe(
    abs = as.list(summary(DIC_res)),
    rel = as.list(summary(DIC_rel))
  ) %>%
  unnest(
    c(abs, rel)
  ) %>%
  `[`(-(1:12*7),) %>%
  mutate(
    stat = rep(names(summary(abs)), 6)
  ) %>%
  pivot_longer(
    cols = c(abs, rel),
    names_to = "res_type",
    values_to = "res_val"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = "res_val"
  ) %>%
  group_by(
    ESPER_model
  ) %>%
  gt(
    row_group_as_column = TRUE
  ) %>%
  tab_header(
    title = "ESPERs DIC Residuals"
  ) %>%
  tab_stubhead(
    label = "Model"
  ) %>%
  cols_label(
    ESPER_input = "Input",
    res_type = "Res. Type",
  ) %>%
  tab_footnote(
    footnote = "lim refers to ESPER calculations performed using only temperature and salinity as predictors; all refers to calculations using all six input variables",
    locations = cells_column_labels(ESPER_input)
  ) %>%
  tab_footnote(
    footnote = "abs = absolute residuals; rel = relative residuals",
    locations = cells_column_labels(res_type)
  ) %>%
  fmt_number(
    decimals = 4
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  gtsave("images/ESPER_comparison/DIC_res_table.png")