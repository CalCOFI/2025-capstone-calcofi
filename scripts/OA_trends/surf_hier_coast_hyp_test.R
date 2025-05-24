
library(tidyverse)
library(lme4)
library(gt)
library(MuMIn)
library(ModelMetrics)

source(here::here("scripts/OA_trends/detrend_data.R"))

merged_bottle_data <- read_csv(here::here("data/merged_bottle_data.csv"))
co2sys_out <- read_csv(here::here("data/CO2SYS_out.csv"))

# Combine merged bottle data and CO2SYS output and filter out anomalies
surf_bottle_co2sys <- bind_cols(merged_bottle_data, co2sys_out) %>%
  filter(
    Salnty > 30,
    Depth <= 20
  )

qty <- c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin")

# Detrend variables of interest
surf_bottle_co2sys <- sea_dtd_data(qty, surf_bottle_co2sys, "Date.cc")

# Average repeated measures

surf_bottle_co2sys <- surf_bottle_co2sys |> group_by(Station_ID, Depth, Date.cc) |>
  select(Station_ID, Depth, Date_Dec, TA_dtd, T_degC_dtd, DIC_dtd, pCO2in_dtd, RFin_dtd, pHin_dtd, CO3in_dtd, OmegaCAin_dtd, OmegaARin_dtd, Salnty_dtd) |> 
  summarize(Station_ID = max(Station_ID),
            Depth = max(Depth),
            Date_Dec = max(Date_Dec),
            TA_dtd = mean(TA_dtd, na.rm = T),
            DIC_dtd = mean(DIC_dtd, na.rm = T),
            T_degC_dtd = mean(T_degC_dtd, na.rm = T),
            pCO2in_dtd = mean(pCO2in_dtd, na.rm = T),
            RFin_dtd = mean(RFin_dtd),
            pHin_dtd = mean(pHin_dtd),
            CO3in_dtd = mean(CO3in_dtd, na.rm = T),
            OmegaCAin_dtd = mean(OmegaCAin_dtd, na.rm = T),
            OmegaARin_dtd = mean(OmegaARin_dtd, na.rm = T),
            Salnty_dtd = mean(Salnty_dtd, na.rm = T)) |> 
  ungroup()

surf_bottle_co2sys$st <- 0
surf_bottle_co2sys$line <- 0
for (i in 1:nrow(surf_bottle_co2sys)){
  surf_bottle_co2sys$st[i] <- str_split(surf_bottle_co2sys$Station_ID[i], " ")[[1]][1]
  surf_bottle_co2sys$line[i] <- str_split(surf_bottle_co2sys$Station_ID[i], " ")[[1]][2]
}
surf_bottle_co2sys <- surf_bottle_co2sys |> 
  mutate(coastal = case_when(
    (as.numeric(st) <= 86) & (as.numeric(line) < 70) ~ TRUE,
    (as.numeric(st) < 90) & (as.numeric(line) < 40) ~ TRUE,
    (as.numeric(st)) >= 90 & (as.numeric(line) < 35) ~ TRUE,
    TRUE ~ FALSE
  ))

# Modeling

# Depth is either included or not based on if it increases AIC 
# models for pH, TA, and Salinity do not include depth

surf_bottle_co2sys <- surf_bottle_co2sys |> 
  filter(!is.na(Date_Dec)) |> 
  mutate(Date_Dec_cen = Date_Dec - min(surf_bottle_co2sys$Date_Dec))

omegaARin_mod <- lmer(
  OmegaARin_dtd ~ Date_Dec + Date_Dec:coastal + Depth + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

T_degC_mod <- lmer(
  T_degC_dtd ~ Date_Dec + Date_Dec:coastal + Depth +  coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

Salnty_mod <- lmer(
  Salnty_dtd ~ Date_Dec + Date_Dec:coastal + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

TA_mod <- lmer(
  TA_dtd ~ Date_Dec + Date_Dec:coastal + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

DIC_mod <- lmer(
  DIC_dtd ~ Date_Dec + Date_Dec:coastal +  Depth + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)



pCO2in_mod <- lmer(
  pCO2in_dtd ~ Date_Dec + Date_Dec:coastal + Depth + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

RFin_mod <- lmer(
  RFin_dtd ~ Date_Dec + Date_Dec:coastal +  Depth + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

pHin_mod <- lmer(
  pHin_dtd ~ Date_Dec + Date_Dec:coastal + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

CO3in_mod <- lmer(
  CO3in_dtd ~ Date_Dec + Date_Dec:coastal +  Depth + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

omegaCAin_mod <- lmer(
  OmegaCAin_dtd ~ Date_Dec + Date_Dec:coastal +  Depth + coastal + (1 | Station_ID),
  data = surf_bottle_co2sys,
  na.action = na.omit,
  REML = FALSE
)

models <- list(TA_mod, DIC_mod, pCO2in_mod, pHin_mod, CO3in_mod, omegaCAin_mod, omegaARin_mod)


# format results into table
lapply(
  1:10,
  function(i) {
    c(qty = qty[i], coef(summary(models[[i]]))[2,], n = nobs(models[[i]]), AIC = AIC(models[[i]]), r2 = r.squaredGLMM(models[[i]])[2],
      CI = paste0("(", format(round(confint(models[[i]])[4,1], 5), nsmall = 5), ", ", format(round(confint(models[[i]])[4,2], 5), nsmall = 5), ")"))
  }
) %>%
  # combine results into a dataframe
  bind_rows() %>%
  # convert appropriate columns to numeric
  mutate(
    across(-c(qty, CI), as.numeric)
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
    title = "Surface Level Mixed Effect Regression Statistics for CalCOFI Stations with Coastal Interaction"
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
    AIC = "AIC",
    CI = "95% CI"
  ) %>%
  # move units to be next to estimate and standard error columns
  cols_move(
    CI,
    after = `Std. Error`
  ) %>%
  cols_move(
    units,
    after = CI
  ) |> 
  fmt_markdown(
    columns = c(qty, CI)
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
  ) |> gtsave("images/OA_trends/hier_surf_coastal.png")


# format results into table
lapply(
  1:10,
  function(i) {
    c(qty = qty[i], coef(summary(models[[i]]))[nrow(coef(summary(models[[i]]))),], n = nobs(models[[i]]), AIC = AIC(models[[i]]), r2 = r.squaredGLMM(models[[i]])[2],
      CI = paste0("(", format(round(confint(models[[i]])[nrow(confint(models[[i]])),1], 5), nsmall = 5), ", ", format(round(confint(models[[i]])[nrow(confint(models[[i]])),2], 5), nsmall = 5), ")"))
  }
) %>%
  # combine results into a dataframe
  bind_rows() %>%
  # convert appropriate columns to numeric
  mutate(
    across(-c(qty, CI), as.numeric)
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
    title = "Surface Level Mixed Effect Regression Statistics for CalCOFI Stations with Coastal Interaction"
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
    Estimate = "Coastal Interaction",
    `Pr(>|t|)` = "p-value",
    `Std. Error` = "Std. Error",
    units = "Units",
    r2 = md("r<sup>2</sup>"),
    AIC = "AIC",
    CI = "95% CI"
  ) %>%
  # move units to be next to estimate and standard error columns
  cols_move(
    CI,
    after = `Std. Error`
  ) %>%
  cols_move(
    units,
    after = CI
  ) |> 
  fmt_markdown(
    columns = c(qty, CI)
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
  ) |> 
gtsave("images/OA_trends/hier_surf_coastal_int.png")


# format results into table
lapply(
  1:7,
  function(i) {
    c(qty = qty[i], int_est = coef(summary(models[[i]]))[nrow(coef(summary(models[[i]]))),1], t_est = coef(summary(models[[i]]))[2,1], n = nobs(models[[i]]), r2 = r.squaredGLMM(models[[i]])[2],
      int_CI = paste0("(", format(round(confint(models[[i]])[nrow(confint(models[[i]])),1], 5), nsmall = 5), ", ", format(round(confint(models[[i]])[nrow(confint(models[[i]])),2], 5), nsmall = 5), ")"),
      t_CI = paste0("(", format(round(confint(models[[i]])[4,1], 5), nsmall = 5), ", ", format(round(confint(models[[i]])[4,2], 5), nsmall = 5), ")"),
      t_p = coef(summary(models[[i]]))[2,5], int_p= coef(summary(models[[i]]))[nrow(coef(summary(models[[i]]))),5])
  }
) %>%
  # combine results into a dataframe
  bind_rows() %>%
  # convert appropriate columns to numeric
  mutate(
    across(-c(qty, t_CI, int_CI), as.numeric)
  ) %>%
  # rename quantities vector for tidier appearance in table
  mutate(
    qty = c("TA", "DIC", "*p*CO2", "pH", "CO~3~<sup>2-</sup>", "Ω~calcite~", "Ω~aragonite~")
  ) %>%
  gt(
    rowname_col = "qty"
  ) %>%
  tab_spanner(
    label = "Temporal Effect",
    columns = c(t_est, t_CI, t_p)
  ) |> 
  tab_spanner(
    label = "Temporal-Coastal Interaction Effect",
    columns = c(int_est, int_CI, int_p)
  ) |> 
  tab_header(
    title = "Carbonate Chemistry Mixed Effect Regression Statistics for CalCOFI Stations"
  ) %>%
  tab_row_group(
    label = "Seawater carbonate chemistry",
    rows = c("DIC", "TA", "*p*CO2")
  ) %>%
  tab_row_group(
    label = "Ocean acidification indicators",
    rows = c("pH", "CO~3~<sup>2-</sup>", "Ω~calcite~", "Ω~aragonite~")
  ) |> 
  # add label to row names
  tab_stubhead(
    label = "Parameter"
  ) %>%
  # rename columns
  cols_label(
    t_est = "Estimate",
    int_est = "Estimate",
    t_p = "p-value",
    int_p = "p-value",
    r2 = md("r<sup>2</sup>"),
    t_CI = "95% CI",
    int_CI = "95% CI"
    
  ) %>%
  fmt_markdown(
    columns = c(qty, t_CI, int_CI)
  ) %>%
  fmt_number(
    columns = c("t_est", "int_est", "t_p", "int_p", "r2"),
    decimals = 4
  ) %>%
  sub_small_vals(
    columns = c(t_p, int_p),
    threshold = 0.0001
  ) %>%
  opt_stylize(
    style = 3
  ) |> 
  tab_options(
    heading.title.font.size = 25
  ) |> 
  gtsave("images/OA_trends/hier_surf_coastal_full.png")
