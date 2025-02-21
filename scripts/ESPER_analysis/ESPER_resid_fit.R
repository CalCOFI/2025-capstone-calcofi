# ESPER_resid_fit.R

### FIT MIXED-EFFECTS MODEL TO ESPER RESIDUALS

library(tidyverse)
library(lme4)
library(lmerTest)

# PREPROCESSING

# read in data
source("scripts/ESPER_analysis/ESPER_out_proc.R")
esper_bottle_combined <- esper_out_proc()

# convert dates to decimal years for fitting
esper_bottle_combined <- esper_bottle_combined %>%
  mutate(
    Date_Dec = decimal_date(Date.cc)
  )

# filter out observations with anomalous salinity
esper_bottle_combined <- esper_bottle_combined %>%
  filter(
    Salnty > 30
  )

# filter out stations that have less than 20 observations
keep_stations <- esper_bottle_combined %>%
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
    x = unique(esper_bottle_combined$Station_ID)
  )
esper_bottle_combined <- esper_bottle_combined %>%
  filter(
    Station_ID %in% keep_stations
  )

# log(1+x) transform depths
esper_bottle_combined <- esper_bottle_combined %>%
  mutate(
    Depth_Trans = log(Depth + 1, base = 10)
  )

# RESIDUALS AGAINST INPUT
TA_all_input_model <- lm(TA_all_res ~ Salnty + T_degC + PO4uM + NO3uM + SiO3uM + `Oxy_µmol/Kg`,
                          data = esper_bottle_combined)
summary(TA_all_input_model)

DIC_all_input_model <- lm(DIC_all_res ~ Salnty + T_degC + PO4uM + NO3uM + SiO3uM + `Oxy_µmol/Kg`,
                          data = esper_bottle_combined)
summary(DIC_all_input_model)

# FIT MIXED-EFFECTS MODEL
TA_lim_model <- lmer(TA_lim_res ~ Date_Dec + Depth_Trans + (Depth_Trans | Station_ID),
                    data = esper_bottle_combined,
                    control = lmerControl(optimizer = "bobyqa"))
summary(TA_lim_model)

TA_all_model <- lmer(TA_all_res ~ Date_Dec + Depth_Trans + (Depth_Trans | Station_ID),
                     data = esper_bottle_combined)
summary(TA_all_model)

DIC_lim_model <- lmer(DIC_lim_res ~ Date_Dec + Depth_Trans + (Depth_Trans | Station_ID),
                     data = esper_bottle_combined)
summary(DIC_lim_model)

DIC_all_model <- lmer(DIC_all_res ~ Date_Dec + Depth_Trans + (Depth_Trans | Station_ID),
                     data = esper_bottle_combined)
summary(DIC_all_model)
