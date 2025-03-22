# ESPER_model_metrics.R

library(tidyverse)
library(gt)

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


# COMPUTE METRICS ---------------------------------------------------------

# calculate standard deviation of observed values
TA_sd <- esper_bottle_combined$TA %>% sd(na.rm = TRUE)
DIC_sd <- esper_bottle_combined$DIC %>% sd(na.rm = TRUE)



# RMSE --------------------------------------------------------------------
# generate table of RMSE values by model and input
esper_bottle_combined %>%
  mutate(
    ESPER_input = factor(ESPER_input, levels = c("lim", "all"), ordered = TRUE)
  ) %>%
  group_by(
    ESPER_model, ESPER_input
  ) %>%
  summarize(
    TA_rmse = sqrt(sum(TA_res^2/sum(!is.na(TA_res)), na.rm = TRUE)),
    DIC_rmse = sqrt(sum(DIC_res^2/sum(!is.na(DIC_res)), na.rm = TRUE)),
    TA_median = median(TA_rel, na.rm = TRUE),
    DIC_median = median(DIC_rel, na.rm = TRUE),
    TA_mean = mean(TA_rel, na.rm = TRUE),
    DIC_mean = mean(DIC_rel, na.rm = TRUE),
    TA_sd = sd(TA_rel, na.rm = TRUE),
    DIC_sd = sd(DIC_rel, na.rm = TRUE)
  ) %>%
  gt(
    row_group_as_column = TRUE
  ) %>%
  tab_header(
    title = "ESPER Error Metrics"
  ) %>%
  tab_stubhead(
    label = "Model"
  ) %>%
  cols_label(
    ESPER_input = "Input",
    TA_rmse = "TA",
    DIC_rmse = "DIC",
    TA_median = "TA",
    DIC_median = "DIC",
    TA_mean = "TA",
    DIC_mean = "DIC",
    TA_sd = "TA",
    DIC_sd = "DIC"
  ) %>%
  tab_spanner(
    label = "RMSE",
    columns = c(TA_rmse, DIC_rmse)
  ) %>%
  tab_spanner(
    label = "Median Error",
    columns = c(TA_median, DIC_median)
  ) %>%
  tab_spanner(
    label = "Mean Error",
    columns = c(TA_mean, DIC_mean)
  ) %>%
  tab_spanner(
    label = "Error SD",
    columns = c(TA_sd, DIC_sd)
  ) %>%
  tab_footnote(
    footnote = "lim refers to ESPER calculations performed using only temperature and salinity as predictors; all refers to calculations using all six input variables",
    locations = cells_column_labels(ESPER_input)
  ) %>%
  tab_footnote(
    footnote = paste0("TA SD = ", signif(TA_sd,7), "; DIC SD = ", signif(DIC_sd, 8)),
    locations = cells_column_labels(c(TA_rmse, DIC_rmse))
  ) %>%
  tab_footnote(
    footnote = "Relative error, i.e. (Predicted - Observed)/Observed",
    locations = cells_column_spanners()
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  fmt_number(
    columns = c(TA_rmse, DIC_rmse),
    decimals = 2
  ) %>%
  fmt_percent(
    columns = c(TA_median, DIC_median, TA_mean, DIC_mean, TA_sd, DIC_sd),
    decimals = 2
  ) %>% gtsave("images/ESPER_analysis/error_metrics_table.png")

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
  ) %>% 
  gtsave("images/ESPER_analysis/rmse_by_depth_table.png")

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
  ) %>% 
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
  geom_hline(
    yintercept = 1,
    lty = 2
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
ggsave("images/ESPER_analysis/rmse_by_year.png", bg = "white")


# RESIDUALS ---------------------------------------------------------------
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
  gtsave("images/ESPER_analysis/TA_res_table.png")

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
  gtsave("images/ESPER_analysis/DIC_res_table.png")

# mean percent error by year
esper_bottle_combined %>%
  group_by(
    ESPER_model, ESPER_input, Year_UTC
  ) %>%
  summarize(
    TA_mean_err = mean(TA_rel, na.rm = TRUE)*100,
    DIC_mean_err = mean(DIC_rel, na.rm = TRUE)*100
  ) %>%
  ungroup() %>%
  add_row(
    ESPER_model = rep(c("LIR", "Mixed", "NN"), each = 12),
    ESPER_input = rep(c("all","lim"), each = 6, length.out = 36),
    Year_UTC = rep(2002:2007,6), 
    TA_mean_err = rep(NA, 36),
    DIC_mean_err = rep(NA, 36)
  ) %>%
  pivot_longer(
    cols = c(TA_mean_err, DIC_mean_err),
    names_to = "qty",
    values_to = "mean_err"
  ) %>%
  ggplot(
    aes(
      x = Year_UTC,
    )
  ) +
  geom_hline(
    yintercept = 0,
    lty = 3
  ) +
  geom_line(
    aes(
      y = mean_err,
      group = qty,
      col = qty
    ),
    na.rm = TRUE
  ) +
  theme_bw() +
  facet_grid(
    ESPER_input ~ ESPER_model
  ) +
  scale_color_discrete(
    labels = c("DIC", "TA"),
    type = c("blue", "red")
  ) +
  labs(
    y = "Mean Percent Error",
    x = "Year",
    col = "Quantity"
  )
ggsave("images/ESPER_analysis/mpe_vs_year.png")