# CO2SYS_out_analysis.R

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
  

# Fit random effects model for each variable of interest
models <- lapply(
  qty,
  function(x) {
    lmer(get(paste0(x,"_dtd")) ~ Date_Dec + Depth_Trans + (0 + Date_Dec | Station_ID),
         data = bottle_co2sys,
         control = lmerControl(optimizer = "nlminbwrap"))
  }
)

summary(models[[1]])
