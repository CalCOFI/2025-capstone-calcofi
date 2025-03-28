# SEE scripts/OA_trends/jade-OA_trends.qmd for comments and notes


library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(gt)
library(MuMIn)
library(ModelMetrics)

source(here::here("scripts/OA_trends/detrend_data.R"))

merged_bottle_data <- read_csv(here::here("data/merged_bottle_data.csv"))
co2sys_out <- read_csv(here::here("data/CO2SYS_out.csv"))

# Combine merged bottle data and CO2SYS output and filter out anomalies
bottle_co2sys <- bind_cols(merged_bottle_data, co2sys_out) %>%
  filter(
    Salnty > 30,
    Depth < 1000
  )

qty <- c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin")

# Detrend variables of interest
bottle_co2sys <- sea_dtd_data(qty, bottle_co2sys, "Date.cc")


# Convert to panel data

multi_seq_bottle_co2sys <- bottle_co2sys |> group_by(Station_ID, Depth, Date.cc) |>
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

multi_seq_bottle_co2sys <- multi_seq_bottle_co2sys |> 
  mutate(depth_bin = case_when(
    Depth < 8 ~ "Surface",
    Depth < 14 ~ "8 - 13 m",
    Depth < 21 ~ "14 - 20 m",
    Depth < 35 ~ "21 - 34 m",
    Depth < 46 ~ "35 - 45 m",
    Depth < 60 ~ "46 - 59 m",
    Depth < 80 ~ "60 - 79 m",
    Depth < 101 ~ "80 - 100 m",
    Depth < 120 ~ "100 - 120 m",
    Depth < 190 ~ "120 - 189 m",
    Depth < 271 ~ "190 - 270 m",
    Depth < 370 ~ "271 - 369 m",
    Depth < 451 ~ "370 - 450 m",
    Depth < 521 ~ "451 ~ 520 m",
    TRUE ~ ">520 m"
  ))

dupes <- multi_seq_bottle_co2sys |>  group_by(Station_ID, depth_bin, Date_Dec) |> 
  arrange(-Date_Dec) |> 
  filter(n() >1) |> 
  ungroup()

multi_seq_bottle_co2sys <- anti_join(multi_seq_bottle_co2sys, dupes)

# Modeling

multi_seq_bottle_co2sys <- multi_seq_bottle_co2sys %>%
  mutate(
    Depth_Trans = log(Depth + 1, base = 10)
  )

multi_seq_bottle_co2sys <- multi_seq_bottle_co2sys |> 
  filter(!is.na(Depth_Trans)) |> 
  filter(!is.na(Date_Dec)) |> 
  mutate(Date_Dec_cen = Date_Dec - min(multi_seq_bottle_co2sys$Date_Dec))

omegaARin_mod <- lme(
  OmegaARin_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  weights = varIdent(form =~1 | depth_bin),
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

T_degC_mod <- lme(
  T_degC_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

Salnty_mod <- lme(
  Salnty_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

TA_mod <- lme(
  TA_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = "optim"),
  na.action = na.omit
)

DIC_mod <- lme(
  DIC_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)



pCO2in_mod <- lme(
  pCO2in_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  weights = varIdent(form =~1 | depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

RFin_mod <- lme(
  RFin_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

pHin_mod <- lme(
  pHin_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

CO3in_mod <- lme(
  CO3in_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

omegaCAin_mod <- lme(
  OmegaCAin_dtd ~ Date_Dec + Depth,
  method = "REML",
  random = ~ 1 | Station_ID,
  weights = varIdent(form =~1 | depth_bin),
  correlation = corCAR1(form=~Date_Dec|Station_ID/depth_bin),
  data = multi_seq_bottle_co2sys,
  control = list(maxIter=10000, niterEM=10000, opt = 'optim'),
  na.action = na.omit
)

models <- list(T_degC_mod, Salnty_mod, TA_mod, DIC_mod, pCO2in_mod, RFin_mod, pHin_mod, CO3in_mod, omegaCAin_mod, omegaARin_mod)


# format results into table
lapply(
  1:10,
  function(i) {
    c(qty = qty[i], coef(summary(models[[i]]))[2,], n = nobs(models[[i]]), r2 = r.squaredGLMM(models[[i]])[2],
      CI = paste0("(", signif((intervals(models[[i]], which = "fixed"))[[1]][2,1], digits = 3), ", ", signif((intervals(models[[i]], which = "fixed")[[1]])[2,3], digits = 3), ")"))
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
    -c("t-value", "DF")
  ) %>%
  gt(
    rowname_col = "qty"
  ) %>%
  tab_header(
    title = "CAR1 Autocorrelation Corrected Mixed Effect Regression Statistics for CalCOFI Stations"
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
    Value = "Slope",
    `p-value` = "p-value",
    `Std.Error` = "Std. Error",
    units = "Units",
    r2 = md("r<sup>2</sup>"),
    CI = "95% CI"
  ) %>%
  # move units to be next to estimate and standard error columns
  cols_move(
    CI,
    after = `Std.Error`
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
    columns = c("Value", "Std.Error", "p-value", "r2"),
    decimals = 4
  ) %>%
  sub_small_vals(
    columns = `p-value`,
    threshold = 0.0001
  ) %>%
  opt_stylize(
    style = 3
  )
