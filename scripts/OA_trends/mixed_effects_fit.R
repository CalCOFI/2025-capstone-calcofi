# mixed_effects_fit.R

library(tidyverse)
library(lme4)
library(lmerTest)
library(gt)
library(MuMIn)
library(ModelMetrics)


# READ IN AND PROCESS DATA ------------------------------------------------

# Load seasonal detrending function
source("scripts/OA_trends/detrend_data.R")

# Load merged bottle data and CO2SYS output
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")
co2sys_out <- read_csv("data/CO2SYS_out.csv")

# Combine merged bottle data and CO2SYS output and filter out anomalies
bottle_co2sys <- bind_cols(merged_bottle_data, co2sys_out) %>%
  filter(
    Salnty > 30
  )

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
    n < 40
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
  

# MODEL SELECTION ---------------------------------------------------------

if (FALSE) {

# create vector of different potential models
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

# create vector to store RMSE of each model
RMSE <- NULL

# fit each model once for each quantity
for (i in 1:length(qty)) {
  for (j in 1:length(formulas)) {
    model <- lmer(as.formula(paste(paste0(qty[i],"_dtd"),"~",rhs[j])),
         data = bottle_co2sys,
         control = lmerControl(optimizer = "nloptwrap"))
    # add RMSE of the fitted model to the RMSE vector
    RMSE <- c(RMSE, rmse(actual = predict(model) - resid(model), predicted = predict(model)))
  }
}

# transform the rmse function into a dataframe and plot RMSE as a function of model complexity
RMSE %>%
  # transform into dataframe
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
  # plot RMSE as a function of model complexity
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
}


# FIT BEST MODEL ----------------------------------------------------------

# fit best model for all ten quantities
models <- lapply(
  qty,
  function(x) {
    lmer(
      as.formula(paste(paste0(x,"_dtd"),"~","Date_Dec + Depth_Trans + (Date_Dec | Station_ID)")),
      data = bottle_co2sys,
      control = lmerControl(optimizer = "Nelder_Mead")
    )
  }
)

# format results into table
lapply(
  1:10,
  function(i) {
    c(qty = qty[i], coef(summary(models[[i]]))[2,], n = nobs(models[[i]]), AIC = extractAIC(models[[i]])[2], r2 = r.squaredGLMM(models[[i]])[2])
  }
) %>%
  # combine results into a dataframe
  bind_rows() %>%
  # convert appropriate columns to numeric
  mutate(
    across(-qty, as.numeric)
  ) %>%
  # rename quantities vector for tidier appearance in table
  mutate(
    qty = c("Temperature", "Salinity", "A~T~", "C~T~", "*p*CO2", "Revelle Factor", "pH", "CO~3~<sup>2-</sup>", "Ω~calcite~", "Ω~aragonite~"),
    # add column of units for each quantity
    units = c("degC yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:atm yr^-1",
              "yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", "yr^-1", "yr^-1")
  ) %>%
  select(
    -c("t value", "df")
  ) %>%
  gt(
    rowname_col = "qty"
  ) %>%
  tab_header(
    title = "Mixed Effect Regression Statistics for CalCOFI Stations with n≥20 Observations"
  ) %>%
  tab_row_group(
    label = "Seawater carbonate chemistry",
    rows = c("C~T~", "A~T~", "*p*CO2", "Revelle Factor")
  ) %>%
  tab_row_group(
    label = "Ocean acidification indicators",
    rows = c("pH", "CO~3~<sup>2-</sup>", "Ω~calcite~", "Ω~aragonite~")
  ) %>%
  tab_row_group(
    label = "Hydrography",
    rows = c("Temperature", "Salinity")
  ) %>%
  # add label to row names
  tab_stubhead(
    label = "Parameter"
  ) %>%
  # rename columns
  cols_label(
    Estimate = "Slope",
    `Pr(>|t|)` = "p-value",
    `Std. Error` = "Std. Error",
    units = "Units",
    r2 = md("r<sup>2</sup>"), 
    AIC = "AIC"
  ) %>%
  # move units to be next to estimate and standard error columns
  cols_move(
    units,
    after = `Std. Error`
  ) %>%
  fmt_markdown(
    columns = qty
  ) %>%
  fmt_units(
    columns = units
  ) %>%
  fmt_number(
    columns = c("Estimate", "Std. Error", "Pr(>|t|)", "r2", "AIC"),
    decimals = 4
  ) %>%
  sub_small_vals(
    columns = `Pr(>|t|)`,
    threshold = 0.0001
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  # save table
  gtsave(
    "images/OA_trends/mixed_effects_res.png"
  )
