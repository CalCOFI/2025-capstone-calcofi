# CO2SYS_out_fit.R

library(tidyverse)
library(lme4)
library(lmerTest)

# Load seasonal detrending function
source("scripts/detrend_data.R")

# Load merged bottle data and CO2SYS output
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")
co2sys_out <- read_csv("data/CO2SYS_out.csv")

# Combine merged bottle data and CO2SYS output
bottle_co2sys <- bind_cols(merged_bottle_data, co2sys_out)

# Create columns for normalized TA and DIC

# Create vector of variables to be detrended
qty <- c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin")

# Detrend variables of interest
bottle_co2sys <- sea_dtd_data(qty, bottle_co2sys, "Date.cc")

# log(1+x) transform depths for fitting
bottle_co2sys <- bottle_co2sys %>%
  mutate(
    Depth_Trans = log(Depth + 1, base = 10)
  )

# Filter out stations with less than 20 observations
keep_stations <- bottle_co2sys %>%
  group_by(
    Station_ID
  ) %>%
  summarize(
    n = n()
  ) %>%
  filter(
    n < 20
  ) %>%
  pull(
    Station_ID
  ) %>%
  setdiff(
    x = unique(bottle_co2sys$Station_ID)
  )
bottle_co2sys <- bottle_co2sys %>%
  filter(
    Station_ID %in% keep_stations
  )
  

# Model Selection
rhs <- c(
  "Date_Dec + (1 | Station_ID)",
  "Date_Dec + (Date_Dec | Station_ID)",
  "Date_Dec + Depth_Trans + (1 | Station_ID)",
  "Date_Dec + Depth_Trans + (Date_Dec | Station_ID)",
  "Date_Dec + Depth_Trans:Date_Dec + (1 | Station_ID)",
  "Date_Dec + Depth_Trans:Date_Dec + (Date_Dec | Station_ID)",
  "Date_Dec + Depth_Trans + Depth_Trans:Date_Dec + (1 | Station_ID)",
  "Date_Dec + Depth_Trans + Depth_Trans:Date_Dec + (Date_Dec | Station_ID)"
)

RMSE <- NULL
library(ModelMetrics)
for (i in 1:length(qty)) {
  for (j in 1:length(formulas)) {
    model <- lmer(as.formula(paste(paste0(qty[i],"_dtd"),"~",rhs[j])),
         data = bottle_co2sys,
         control = lmerControl(optimizer = "nloptwrap"))
    RMSE <- c(RMSE, rmse(actual = predict(model) - resid(model), predicted = predict(model)))
  }
}
RMSE %>%
  matrix(
    nrow = length(qty), byrow = TRUE
  ) %>%
  data.frame() %>%
  mutate(
    qty = qty
  ) %>%
  pivot_longer(
    cols = -qty,
    names_to = "formula",
    values_to = "rmse",
    names_transform = function(x) {gsub("X([0-9]+)", "\\1", x)}
  ) %>%
  ggplot(
    aes(
      x = formula,
      y = rmse,
      group = qty,
      col = qty
    )
  ) +
  geom_line() +
  scale_y_log10() +
  theme_minimal()

# Fit best model
models <- lapply(
  qty,
  function(x) {
    lmer(
      as.formula(paste(paste0(x,"_dtd"),"~","Date_Dec + Depth_Trans + (1 | Station_ID)")),
      data = bottle_co2sys,
      control = lmerControl(optimizer = "Nelder_Mead")
    )
  }
)

# Format results
lapply(
  1:length(models),
  function(x) {
    models[[x]] %>% 
      summary() %>% 
      coef() %>% 
      `[`(2,c(1,2,5)) %>% 
      c(qty = qty[x], .) %>% 
      t() %>%
      data.frame()
  }
) %>% bind_rows() %>% 
  rename(
    est = Estimate,
    std = Std..Error,
    pval = Pr...t..
  ) %>%
  mutate(
    across(
      -qty,
      as.numeric
    ),
    qty = factor(qty, levels = qty, labels = c("Temperature", "Salinity", "TA", "DIC",
                                               "pCO2", "Revelle Factor", "pH", "CO3",
                                               "OmegaCA", "OmegaAR")),
    units = c("degC yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:atm yr^-1",
              "yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", "yr^-1", "yr^-1"),
    .before = pval
  ) %>%
  gt() %>%
  tab_header(
    "Estimated Rates of Change for Ocean Carbonate System Quantities"
  ) %>%
  cols_merge_uncert(
    est,
    std
  ) %>%
  cols_merge(
    columns = c(est, units)
  ) %>%
  cols_label(
    qty = "Quantity",
    est = "Estimated Annual Change",
    pval = "p-value"
  ) %>%
  fmt_number(
    columns = c(est, std),
    decimals = 4
  ) %>%
  fmt_scientific(
    columns = pval,
    decimals = 4
  ) %>%
  fmt_units(
    columns = units
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  tab_footnote(
    locations = cells_body(columns = qty, rows = 1:4),
    footnote = "Observed quantities in CalCOFI bottle data"
  ) %>%
  tab_footnote(
    locations = cells_body(columns = qty, rows = 5:10),
    footnote = "Derived quantities from CO2SYS"
  ) %>%
  gtsave(
    "images/CO2SYS_out_fit/rates_table.png"
  )
