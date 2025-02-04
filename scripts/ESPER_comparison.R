# ESPER_comparison.R

### COMPARE OBSERVED TA, TC IN MERGED BOTTLE DATA TO ESPER PREDICTIONS 

library(tidyverse)
library(latex2exp)

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

# fit DIC_lim residuals
DIC_lim_fit <- esper_bottle_combined %>%
  mutate(
    Date_Dec = date_decimal(Date)
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
    Date_Dec = date_decimal(Date)
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
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = DIC,
      color = "Observations",
      shape = "Observations"
    ),
    na.rm = TRUE
  ) +
  geom_point(
    aes(
      y = DIC_lim,
      shape = "ESPER",
      color = "ESPER"
    ),
    na.rm = TRUE
  ) +
  theme_minimal() +
  scale_color_discrete(
    breaks = c("Observations", "ESPER"), type = c("blue", "black")
  ) +
  scale_shape_manual(
    breaks = c("Observations", "ESPER"), values = c(1, 4)
  ) +
  guides(color=guide_legend(), shape=guide_legend()) + 
  labs(
    shape = "Legend",
    color = "Legend",
    x = "",
    y = TeX("$C_T$"),
  )
ggsave("images/ESPER_comparison/CT_lim.png")

esper_bottle_combined %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = DIC_lim_res,
    ),
    shape = 1,
    na.rm = TRUE
  ) + 
  geom_hline(
    yintercept = 0,
    lty = 2
  ) + 
  theme_minimal() +
  labs(
    x = "",
    y = TeX("$C_T$ Residuals (Obs - ESPER)")
  )
ggsave("images/ESPER_comparison/CT_lim_res.png")

esper_bottle_combined %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = DIC,
      color = "Observations",
      shape = "Observations"
    ),
    na.rm = TRUE
  ) +
  geom_point(
    aes(
      y = DIC_all,
      shape = "ESPER",
      color = "ESPER"
    ),
    na.rm = TRUE
  ) +
  theme_minimal() +
  scale_color_discrete(
    breaks = c("Observations", "ESPER"), type = c("blue", "black")
  ) +
  scale_shape_manual(
    breaks = c("Observations", "ESPER"), values = c(1, 4)
  ) +
  guides(color=guide_legend(), shape=guide_legend()) + 
  labs(
    shape = "Legend",
    color = "Legend",
    x = "",
    y = TeX("$C_T$")
  )
ggsave("images/ESPER_comparison/CT_all.png")

esper_bottle_combined %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = DIC_all_res,
    ),
    shape = 1,
    na.rm = TRUE
  ) + 
  geom_hline(
    yintercept = 0,
    lty = 2
  ) + 
  theme_minimal() +
  labs(
    x = "",
    y = TeX("$C_T$ Residuals (Obs - ESPER)")
  )
ggsave("images/ESPER_comparison/CT_all_res.png")

esper_bottle_combined %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = TA,
      color = "Observations",
      shape = "Observations"
    ),
    na.rm = TRUE
  ) +
  geom_point(
    aes(
      y = TA_lim,
      shape = "ESPER",
      color = "ESPER"
    ),
    na.rm = TRUE
  ) +
  theme_minimal() +
  scale_color_discrete(
    breaks = c("Observations", "ESPER"), type = c("blue", "black")
  ) +
  scale_shape_manual(
    breaks = c("Observations", "ESPER"), values = c(1, 4)
  ) +
  guides(color=guide_legend(), shape=guide_legend()) + 
  labs(
    shape = "Legend",
    color = "Legend",
    x = "",
    y = TeX("$A_T$"),
  )
ggsave("images/ESPER_comparison/AT_lim.png")

esper_bottle_combined %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = TA_lim_res,
    ),
    shape = 1,
    na.rm = TRUE
  ) + 
  geom_hline(
    yintercept = 0,
    lty = 2
  ) + 
  theme_minimal() +
  labs(
    x = "",
    y = TeX("$A_T$ Residuals (Obs - ESPER)")
  )
ggsave("images/ESPER_comparison/AT_lim_res.png")

esper_bottle_combined %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = TA,
      color = "Observations",
      shape = "Observations"
    ),
    na.rm = TRUE
  ) +
  geom_point(
    aes(
      y = TA_all,
      shape = "ESPER",
      color = "ESPER"
    ),
    na.rm = TRUE
  ) +
  theme_minimal() +
  scale_color_discrete(
    breaks = c("Observations", "ESPER"), type = c("blue", "black")
  ) +
  scale_shape_manual(
    breaks = c("Observations", "ESPER"), values = c(1, 4)
  ) +
  guides(color=guide_legend(), shape=guide_legend()) + 
  labs(
    shape = "Legend",
    color = "Legend",
    x = "",
    y = TeX("$A_T$"),
  )
ggsave("images/ESPER_comparison/AT_all.png")

esper_bottle_combined %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date
    )
  ) +
  geom_point(
    aes(
      y = TA_all_res,
    ),
    shape = 1,
    na.rm = TRUE
  ) + 
  geom_hline(
    yintercept = 0,
    lty = 2
  ) + 
  theme_minimal() +
  labs(
    x = "",
    y = TeX("$A_T$ Residuals (Obs - ESPER)")
  )
ggsave("images/ESPER_comparison/AT_all_res.png")