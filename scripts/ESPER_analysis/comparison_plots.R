# comparison_plots.R

### COMPARE OBSERVED TA, TC IN MERGED BOTTLE DATA TO ESPER PREDICTIONS 

library(tidyverse)
library(ggforce)
library(scales)
library(ModelMetrics)
library(gt)
library(cowplot)
library(latex2exp)

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


# COMPARE PREDICTIONS AGAINST OBSERVATIONS --------------------------------

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
    alpha = 0.5
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
ggsave("images/ESPER_analysis/DIC_pred_vs_obs.png", bg = "white")

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
    alpha = 0.5
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
ggsave("images/ESPER_analysis/TA_pred_vs_obs.png", bg = "white")

# Mixed-all plots
esper_bottle_combined %>%
  filter(
    (Salnty > 30) & (ESPER_model == "Mixed") & (ESPER_input == "all")
  ) %>%
  rename(
    DIC_obs = DIC,
    TA_obs = TA
  ) %>%
  pivot_longer(
    cols = c(DIC_obs, TA_obs, DIC_est, TA_est),
    names_to = c("qty", "type"),
    values_to = "value",
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = value,
    values_fn = list
  ) %>%
  unnest(
    c = c(obs, est)
  ) %>%
  mutate(
    qty = factor(qty, levels = c("TA", "DIC"))
  ) %>%
  ggplot(
    aes(
      x = obs,
      y = est,
      col = Depth
    )
  ) +
  geom_point(
    na.rm = TRUE,
    alpha = 0.5
  ) + 
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  scale_color_gradient(
    trans = trans_reverser("pseudo_log"),
    breaks = c(1,10,100,1000)
  ) + 
  facet_wrap(
    vars(qty),
    scales = "free"
  ) +
  theme_bw() +
  labs(
    x = TeX("Observed ($\\mu{mol}$ {kg}$^{-1}$\ {yr}$^{-1}$)"),
    y = TeX("Predicted ($\\mu{mol}$ {kg}$^{-1}$\ {yr}$^{-1}$)"),
    title = "ESPER Mixed Predicted vs. Observed Values",
    color = "Depth (m)"
  ) +
  theme(
    text = element_text(size = 20)
  )
ggsave("images/ESPER_analysis/ESPER_Mixed_all_preds_v_obs.png", width = 10, height = 6, units = 'in', bg = "white",dpi=600)


# COMPARE ABSOLUTE RESIDUALS AGAINST OBSERVED -----------------------------

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
ggsave("images/ESPER_analysis/DIC_res_v_obs.png", bg = "white")

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
ggsave("images/ESPER_analysis/TA_res_v_obs.png", bg = "white")

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
        y = DIC_rel,
        col = input_var
      )
    ) +
    geom_point(
      na.rm = TRUE,
      alpha = 0.3
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
ggsave("images/ESPER_analysis/DIC_res_v_input.png", scale = 2, bg = "white")

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
        y = TA_rel,
        col = input_var
      )
    ) +
    geom_point(
      na.rm = TRUE,
      alpha = 0.3
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
ggsave("images/ESPER_analysis/TA_res_v_input.png", scale = 2, bg = "white")


# ESPER NN specific plots
DIC_input_plots[[5]] +
  labs(
    title = "ESPER NN Relative DIC Residuals against Input Variables"
  )
ggsave("images/ESPER_analysis/ESPER_NN_DIC_res_v_input.png", bg = "white")
TA_input_plots[[5]] +
  labs(
    title = "ESPER NN Relative TA Residuals against Input Variables"
  )
ggsave("images/ESPER_analysis/ESPER_NN_TA_res_v_input.png", bg = "white")

# COMPARE RESIDUALS AGAINST DEPTH, TIME -----------------------------------

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
ggsave("images/ESPER_analysis/DIC_res_v_depth.png", bg = "white")

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
ggsave("images/ESPER_analysis/TA_res_v_depth.png", bg = "white")

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
ggsave("images/ESPER_analysis/DIC_res_v_date.png", bg = "white")

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
ggsave("images/ESPER_analysis/TA_res_v_date.png", bg = "white")