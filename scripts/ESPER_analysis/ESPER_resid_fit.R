# ESPER_resid_fit.R

### FIT MIXED-EFFECTS MODEL TO ESPER RESIDUALS

library(tidyverse)
library(lme4)

# PREPROCESSING

# read in data
source("scripts/ESPER_analysis/ESPER_out_proc.R")
esper_bottle_combined <- esper_out_proc()

# convert dates to decimal years for fitting
esper_bottle_combined <- esper_bottle_combined %>%
  mutate(
    Date_Dec = decimal_date(Date.cc)
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
    n < 30
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

# FITTING
TA_lim_model <- lmer(TA_lim_res ~ Date_Dec + Depth + (1 | Station_ID),
                     data = esper_bottle_combined)
summary(TA_lim_model)

TA_all_model <- lmer(TA_all_res ~ Date_Dec + Depth + (1 | Station_ID),
                     data = esper_bottle_combined)
summary(TA_all_model)

DIC_lim_model <- lmer(DIC_lim_res ~ Date_Dec + Depth + (1 | Station_ID),
                     data = esper_bottle_combined)
summary(DIC_lim_model)

DIC_all_model <- lmer(DIC_all_res ~ Date_Dec + Depth + (1 | Station_ID),
                     data = esper_bottle_combined)
summary(DIC_all_model)