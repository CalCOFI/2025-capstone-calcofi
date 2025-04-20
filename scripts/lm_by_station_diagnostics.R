# lm_by_station.R

library(tidyverse)
library(gt)
library(sf)
library(rnaturalearth)
library(scales)
library(latex2exp)


# READ AND PROCESS DATA ---------------------------------------------------

# Load seasonal detrending function
source("scripts/OA_trends/detrend_data.R")

# Load merged bottle data and CO2SYS output
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")
co2sys_out <- read_csv("data/CO2SYS_out.csv")

# Combine merged bottle data and CO2SYS output and filter out anomalies
bottle_co2sys <- bind_cols(merged_bottle_data, co2sys_out) %>%
  filter(
    Salnty > 30,
    Depth < 1000
  )

# Create vector of variables to be detrended
qty <- c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin")

# Detrend variables of interest
bottle_co2sys <- sea_dtd_data(qty, bottle_co2sys, "Date.cc")

# log(1+x) transform depths for fitting
bottle_co2sys <- bottle_co2sys %>%
  mutate(
    Depth_Log1 = log(Depth + 1),
    Depth_Log10 = log(Depth + 10),
    Depth_Log50 = log(Depth + 50),
    Depth_Log200 = log(Depth + 200)
  )

# Get the names of all CalCOFI stations in the data and their locations
stations <- bottle_co2sys %>%
  group_by(
    Station_ID
  ) %>%
  summarize(
    lat = mean(Latitude),
    lon = mean(Longitude),
  )

# FIT LINEAR MODELS -------------------------------------------------------

# create fits and results object
fits <- NULL
results <- NULL

depth_dep <- c("Depth_Log10", "Depth_Log50", "Depth", "Depth_Log50", "Depth_Log50",
               "Depth_Log1", "Depth_Log10", "Depth_Log1", "Depth_Log1", "Depth_Log1")

# iterate through stations and fit linear models for each quantity
for (i in 1:nrow(stations)) {
  # extract data for station i
  data <- bottle_co2sys %>% filter(Station_ID == stations$Station_ID[i])
  for (j in 1:length(qty)) {
    # check if all values are NA
    if (data %>% select(paste0(qty[j],"_dtd")) %>% is.na() %>% `!`() %>% sum() == 0) {
      # if so, add NA to list of fits
      fits[[(i-1)*length(qty)+j]] <- NA
      # and add row of NA values to results for the corresponding quantity and station
      results <- bind_rows(results, c(
        station = stations$Station_ID[i],
        lat = stations$lat[i],
        lon = stations$lon[i],
        qty = qty[j], 
        c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA), 
        n = NA, 
        r2 = NA))
    }
    else { # fit the linear model
      fit <- lm(as.formula(paste(paste0(qty[j],"_dtd"),"~","Date_Dec +", depth_dep[j])), data = data, na.action = na.exclude)
      # add fit to list of fits
      fits[[(i-1)*length(qty)+j]] <- fit
      # add coefficient estimate and regression statistics in a new row to results
      results <- bind_rows(results, c(
        station = stations$Station_ID[i],
        lat = stations$lat[i],
        lon = stations$lon[i], 
        qty = qty[j], 
        if(nrow(coef(summary(fit))) == 1) c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA) else coef(summary(fit))[2,], 
        n = summary(fit)$df[2] + 2, 
        r2 = summary(fit)$r.squared))
    }
  }
}


# create fits and results objects for surface (<20m)
surf_fits <- NULL
surf_results <- NULL

# iterate through stations and fit linear models for each quantity
for (i in 1:nrow(stations)) {
  # extract data for station i
  data <- bottle_co2sys %>% filter((Station_ID == stations$Station_ID[i]) & (Depth <= 20))
  for (j in 1:length(qty)) {
    # check if all values are NA
    if (data %>% select(paste0(qty[j],"_dtd")) %>% is.na() %>% `!`() %>% sum() == 0) {
      # if so, add NA to list of fits
      surf_fits[[(i-1)*length(qty)+j]] <- NA
      # and add row of NA values to surf_results for the corresponding quantity and station
      surf_results <- bind_rows(surf_results, c(
        station = stations$Station_ID[i],
        lat = stations$lat[i],
        lon = stations$lon[i],
        qty = qty[j], 
        c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA), 
        n = NA, 
        r2 = NA))
    }
    else { # fit the linear model
      fit <- lm(as.formula(paste(paste0(qty[j],"_dtd"),"~","Date_Dec")), data = data, na.action = na.exclude)
      # add fit to list of fits
      surf_fits[[(i-1)*length(qty)+j]] <- fit
      # add coefficient estimate and regression statistics in a new row to surf_results
      surf_results <- bind_rows(surf_results, c(
        station = stations$Station_ID[i],
        lat = stations$lat[i],
        lon = stations$lon[i], 
        qty = qty[j], 
        if(nrow(coef(summary(fit))) == 1) c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA) else coef(summary(fit))[2,], 
        n = summary(fit)$df[2] + 2, 
        r2 = summary(fit)$r.squared))
    }
  }
}

# Diagnostics

for (i in seq_along(surf_fits)) {
  model <- surf_fits[[i]]
  if (is.na(coef(summary(surf_fits[[i]]))[1,2])){
    next
  }
  df <- data.frame(Fitted = fitted(model)[[i]], Residuals = resid(model)[[i]])
  if (nrow(df) == 1){
    next
  }
  
  p <- ggplot(df) +
    geom_point(aes(x = Fitted, y = Residuals)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle(paste("Residual Plot for Model", i)) +
    theme_minimal()
  
  print(p)  # Needed when plotting inside a loop
}
