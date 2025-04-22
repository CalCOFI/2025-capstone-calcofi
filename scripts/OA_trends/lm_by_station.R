# lm_by_station.R

library(tidyverse)
library(gt)
library(sf)
library(rnaturalearth)
library(scales)
library(latex2exp)
library(FDRestimation)


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

# Filter for surface data
bottle_co2sys <- bottle_co2sys %>%
  filter(
    Depth >= 20
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

# Get the names of a subset of CalCOFI stations and their locations
min_unique <- 8 # minimum number of unique years per station
min_n <- 100 # minimum number of observations per station
stations <- bottle_co2sys %>%
  filter(
    if_all(c(T_degC_dtd, Depth, Date_Dec), ~!is.na(.x))
  ) %>%
  group_by(
    Station_ID
  ) %>%
  summarize(
    length = max(Year_UTC) - min(Year_UTC),
    n = n(),
    unique = length(unique(Year_UTC)),
    lat = mean(Latitude),
    lon = mean(Longitude)
  ) %>% 
  filter(
    unique >= 8,
    n >= 100
  )

# FIT LINEAR MODELS -------------------------------------------------------

# create fits and results object
fits <- NULL
results <- NULL

# iterate through stations and fit linear models for each quantity
for (i in 1:nrow(stations)) {
  # extract data for station i
  data <- bottle_co2sys %>% filter((Station_ID == stations$Station_ID[i]))
  for (j in 1:length(qty)) {
    # fit the linear model
    fit <- lm(as.formula(paste(paste0(qty[j],"_dtd"),"~","Date_Dec")), data = data, na.action = na.exclude)
    
    # add fit to list of fits
    fits[[(i-1)*length(qty)+j]] <- fit
    
    # add coefficient estimate and regression statistics in a new row to surf_results
    results <- bind_rows(
      results, 
      c(
        station = stations$Station_ID[i],
        lat = stations$lat[i],
        lon = stations$lon[i], 
        qty = qty[j], 
        if(nrow(coef(summary(fit))) == 1) c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA) else coef(summary(fit))[2,], 
        n = summary(fit)$df[2] + 2
      )
    )
  }
}

for (i in 1:length(fits)) {
  print(
    ggplot() +
      geom_point(
        aes(
          x = fitted(fits[[i]]),
          y = resid(fits[[i]])
        )
      )
  )
}


# PLOT FIT RESULTS ----------------------------------------------------

# modify results objects for plotting
results <- results %>%
  # convert numeric columns to numeric vectors
  mutate(
    across(-c(station, qty), as.numeric)
  ) %>%
  # implement multiple testing correction
  mutate(
    adj_p_value = (p.fdr(pvalues = .data$`Pr(>|t|)`))$fdrs
  ) %>%
  # create vector indicating if p < 0.5
  mutate(
    sigp = factor(ifelse(`Pr(>|t|)` < 0.5, 1, 0), levels = c(1,0), labels = c("Yes", "No"))
  )

# import map for plotting
world <- ne_countries(scale = "medium", returnclass = "sf")

# create vector of (full) names for each quantity
qty_names <- c("Temperature", "Salinity", "TA", "DIC", "pCO2", "Revelle Factor", "pH", "CO3$^{2-}$", "$\\Omega_{calcite}$", "$\\Omega_{aragonite}$")

# create vector of units for each quantity
units <- c("$^\\circ$C yr$^{-1}$", "yr$^{-1}$", "µmol kg$^{-1}$ yr$^{-1}$", 
           "µmol kg$^{-1}$ yr$^{-1}$", "µatm yr$^{-1}$","yr$^{-1}$", 
           "yr$^{-1}$", "µmol kg$^{-1}$ yr$^{-1}$", "yr$^{-1}$", "yr$^{-1}$")

# generate a plot of slope by station for each quantity
for (i in 1:10) {
  # extract data for quantity i
  data <- results %>%
    filter(
      qty == qty[i]
    ) %>%
    # filter out stations with n<=30 observations used in the fit
    filter(
      (!is.na(Estimate)) & (n > 30)
    )
  
  # create plot of slope by station
  print(
  ggplot(
    data = world
  ) +
    geom_sf(fill = "antiquewhite1") +
    geom_point(
      data = data,
      aes(
        x = lon,
        y = lat,
        fill = Estimate, # estimated slope
        shape = sigp # if estimate is statistically significant
      ),
      size = 8,
      color = "black",
      show.legend=TRUE # force shape to always show in legend
    ) +
    # manually adjust coordinates
    coord_sf(
      xlim = c(results$lon %>% min() - 2, results$lon %>% max() + 2),
      ylim = c(results$lat %>% min(), results$lat %>% max())
    ) +
    # create color scale for slope estimates
    scale_fill_gradient2(
      low = "#d7191c",
      high = "#2c7bb6",
      mid = "#ffffbf"
    ) +
    # create custom shape scale
    scale_shape_manual(
      values = c("Yes" = 24, "No" = 21),
      drop = FALSE # force both shapes to always show in legend
    ) +
    theme(
      panel.grid.major = element_line(
        color = gray(0.5), 
        linetype = "solid", 
        linewidth = 0.5
      ), 
      panel.background = element_rect(fill = "aliceblue")
    ) +
    # fix the order of the legends
    guides(
      fill = guide_colorbar(order = 1),
      size = guide_legend(order = 50),
      shape = guide_legend(order = 98)
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = TeX(paste("Estimated Slope for", qty_names[i], "by Station (Depth >= 20m)", paste0("[",units[i],"]"))),
      color = "Estimate",
      size = "N",
      shape = TeX("$p<0.05$"),
      caption = TeX(paste("Mean Slope (weighted by $N$):", format(round(weighted.mean(data$Estimate, data$n), 4), nsmall = 4), units[i]))
    ))
  
  # save plots
  ggsave(paste0("images/OA_trends/", qty[i], "_by_station.png"), bg = "white")
}

# GENERATE TABULAR SUMMARY ------------------------------------------------

results %>%
  filter(
    n>30
  ) %>%
  group_by(
    qty
  ) %>%
  summarize(
    mean = weighted.mean(Estimate, n, na.rm = TRUE),
    std = sd(Estimate, na.rm = TRUE),
    min = min(Estimate, na.rm = TRUE),
    max = max(Estimate, na.rm = TRUE),
    n = sum(!is.na(Estimate))
  ) %>%
  arrange(
    match(qty, c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin"))
  ) %>%
  mutate(
    qty = c("Temperature", "Salinity", "A~T~", "C~T~", "*p*CO2", "Revelle Factor", "pH", "CO~3~<sup>2-</sup>", "Ω~calcite~", "Ω~aragonite~"),
    units = c("degC yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:atm yr^-1",
              "yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", "yr^-1", "yr^-1")
  ) %>%
  gt(
    rowname_col = "qty"
  ) %>%
  tab_header(
    title = "Summary of By Station Regression Results (Depth >= 20m)"
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
  tab_stubhead(
    label = "Parameter"
  ) %>%
  cols_label(
    mean = "Mean",
    std = "Std. Dev.",
    min = "Min",
    max = "Max",
    units = "Units",
    n = "No. of Stations"
  ) %>%
  cols_move(
    units,
    after = max
  ) %>%
  fmt_markdown(
    columns = qty
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  fmt_units(
    columns = units
  ) %>%
  fmt_number(
    columns = c("mean", "std", "min", "max"),
    decimals = 4
  ) %>%
  gtsave(
    "images/OA_trends/lm_by_station_tab.png"
  )