# weighted_lm_by_station.R

# THERE IS SOME ERROR WITH THE PLOTTING SECTION THAT MUST BE CORRECTED

library(tidyverse)
library(gt)
library(sf)
library(rnaturalearth)
library(scales)
library(latex2exp)
library(nlme)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ModelMetrics)
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

# Create vector of variables to be detrended
qty <- c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin")

# Detrend variables of interest
bottle_co2sys <- sea_dtd_data(qty, bottle_co2sys, "Date.cc")

bottle_co2sys <- bottle_co2sys |> group_by(Station_ID, Depth, Date.cc) |>
  select(Station_ID, Depth, Date_Dec, TA_dtd, T_degC_dtd, DIC_dtd, pCO2in_dtd, Latitude, Longitude, RFin_dtd, pHin_dtd, CO3in_dtd, OmegaCAin_dtd, OmegaARin_dtd, Salnty_dtd) |> 
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
            Salnty_dtd = mean(Salnty_dtd, na.rm = T),
            Latitude = mean(Latitude, na.rm = T),
            Longitude = mean(Longitude, na.rm = T)) |> 
  ungroup()

bottle_co2sys <- bottle_co2sys |> 
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

keep_stations <- bottle_co2sys %>%
  group_by(
    Station_ID
  ) %>%
  summarize(
    n = n()
  ) %>%
  filter(
    n <= 30
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
for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(OmegaARin_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "OmegaARin", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}


for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(TA_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "TA", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(OmegaCAin_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "OmegaCAin", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(CO3in_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "CO3in", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(pHin_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "pHin", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(RFin_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "RFin", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(pCO2in_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "pCO2in", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(Salnty_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "Salnty", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(T_degC_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "T_degC", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

for (i in 1:nrow(stations)){
  data <- bottle_co2sys |> filter(Station_ID == stations$Station_ID[i])
  fit <- gls(DIC_dtd ~ Date_Dec + Depth,
             data = data,
             na.action = na.omit,
             weights = varIdent(form =~1 | depth_bin),
             control = list(maxIter=10000, niterEM=10000, opt = 'optim')
  )
  fits[[i]] <- fit
  results <- bind_rows(results, tibble(station = stations$Station_ID[i],
                                  lat = as.double(stations$lat[i]),
                                  lon = as.double(stations$lon[i]), 
                                  qty = "DIC", 
                                  Value =coef(summary(fit))[2,1],
                                  Std.Error =coef(summary(fit))[2,2],
                                  `t-value` =coef(summary(fit))[2,3],
                                  `p-value` =coef(summary(fit))[2,4],
                                  n = length(fit$fitted),
                                  AIC = summary(fit)$AIC))
}

# # iterate through stations and fit linear models for each quantity
# for (i in 1:nrow(stations)) {
#   # extract data for station i
#   data <- bottle_co2sys %>% filter(Station_ID == stations$Station_ID[i])
#   for (j in 1:length(qty)) {
#     # check if all values are NA
#     if (data %>% select(paste0(qty[j],"_dtd")) %>% is.na() %>% `!`() %>% sum() == 0) {
#       # if so, add NA to list of fits
#       fits[[(i-1)*length(qty)+j]] <- NA
#       # and add row of NA values to results for the corresponding quantity and station
#       results <- bind_rows(results, c(
#         station = stations$Station_ID[i],
#         lat = stations$lat[i],
#         lon = stations$lon[i],
#         qty = qty[j], 
#         c(Value = NA, `Std.Error` = NA, `t-value` = NA, `p-value` = NA), 
#         n = NA, 
#         AIC = NA))
#     }
#     else { # fit the linear model
#       fit <- gls(as.formula(paste(paste0(qty[j],"_dtd"),"~","Date_Dec + Depth")),
#                 data = data,
#                 na.action = na.omit,
#                 weights = varIdent(form =~1 | depth_bin),
#                 control = list(maxIter=10000, niterEM=10000, opt = 'optim')
#                 )
#       # add fit to list of fits
#       fits[[(i-1)*length(qty)+j]] <- fit
#       # add coefficient estimate and regression statistics in a new row to results
#       results <- bind_rows(results, c(
#         station = stations$Station_ID[i],
#         lat = stations$lat[i],
#         lon = stations$lon[i], 
#         qty = qty[j], 
#         Value =coef(summary(fit))[2,1],
#         Std.Error =coef(summary(fit))[2,2],
#         `t-value` =coef(summary(fit))[2,3],
#         `p-value` =coef(summary(fit))[2,4],
#         n = length(fit$fitted),
#         AIC = summary(fit)$AIC))
#     }
#   }
# }


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
        r2 = NA,
        AIC = NA))
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
        r2 = summary(fit)$r.squared,
        AIC = extractAIC(fit)[2]))
    }
  }
}

# PLOT FIT RESULTS ----------------------------------------------------

# modify results objects for plotting

# Benjamini-Hochberg Adjusted p-values
surf_results <- surf_results %>%
  # convert numeric columns to numeric vectors
  mutate(
    across(-c(station, qty), as.numeric))
    
results <- results |> 
  mutate(surface = 0)

surf_results <- surf_results |> 
  mutate(surface = 1) |> 
  rename(Value = Estimate, Std.Error = `Std. Error`, `t-value` = `t value`, `p-value` = `Pr(>|t|)`) |> 
  select(-r2)

total_results <- bind_rows(results, surf_results)

total_results <- total_results |> 
  mutate(adj_p_value = (p.fdr(pvalues = total_results$`p-value`))$fdrs)

results <- total_results |> 
  filter(surface == 0)

surf_results <- total_results |> 
  filter(surface == 1)

results <- results %>%
  # convert numeric columns to numeric vectors
  mutate(
    across(-c(station, qty), as.numeric)
  ) %>%
  # create vector indicating if p < 0.05
  mutate(
    sigp = factor(ifelse(adj_p_value < 0.05 , 1, 0), levels = c(1,0), labels = c("Yes", "No"))
  )
surf_results <- surf_results %>%
  # convert numeric columns to numeric vectors
  mutate(
    across(-c(station, qty), as.numeric)
  ) %>%
  # create vector indicating if p < 0.5
  mutate(
    sigp = factor(ifelse(adj_p_value < 0.05, 1, 0), levels = c(1,0), labels = c("Yes", "No"))
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
      (!is.na(Value)) & (n > 30)
    )
  
  # create plot of slope by station
  ggplot(
    data = world
  ) +
    geom_sf(fill = "antiquewhite1") +
    geom_point(
      data = data,
      aes(
        x = lon,
        y = lat,
        fill = Value, # estimated slope
        size = n, # number of observations
        shape = sigp # if estimate is statistically significant
      ),
      color = "black",
      show.legend=TRUE # force shape to always show in legend
    ) +
    geom_text(data = data, nudge_y = -.07 , size = 1.4, aes(x = lon, y = lat, label=paste("(",station,",",signif(Estimate, digits = 4), ")"))) + 
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
      title = TeX(paste("Estimated Slope for", qty[i], "by Station (N>30)", paste0("[",units[i],"]"))),
      color = "Estimate",
      size = "N",
      shape = TeX("$Significance$"),
      caption = TeX(paste("Mean Slope (weighted by $N$):", format(round(weighted.mean(data$Value, data$n), 4), nsmall = 4), units[i]))
    )
  
  # save plots
  ggsave(paste0("images/OA_trends/", qty[i], "_by_station_weighted.png"), bg = "white")
}

# generate plots for surface fits
for (i in 1:10) {
  # extract data for quantity i
  data <- surf_results %>%
    filter(
      qty == qty[i]
    ) %>%
    # filter out stations with n<=15 observations used in the fit
    filter(
      (!is.na(Value)) & (n > 15)
    )
  
  # create plot of slope by station
  ggplot(
    data = world
  ) +
    geom_sf(fill = "antiquewhite1") +
    geom_point(
      data = data,
      aes(
        x = lon,
        y = lat,
        fill = Value, # estimated slope
        size = n, # number of observations
        shape = sigp # if estimate is statistically significant
      ),
      color = "black",
      show.legend=TRUE # force shape to always show in legend
    ) +
    geom_text(data = data, aes(x= lon, y = lat, label=paste("(",station,",",Value, ")"))) +
    # manually adjust coordinates
    coord_sf(
      xlim = c(surf_results$lon %>% min() - 2, surf_results$lon %>% max() + 2),
      ylim = c(surf_results$lat %>% min(), surf_results$lat %>% max())
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
      title = TeX(paste("Estimated Slope for", qty[i], "by Station at Surface (Depth<=20m, N>15)", paste0("[",units[i],"]"))),
      color = "Estimate",
      size = "N",
      shape = TeX("$p<0.5$"),
      caption = TeX(paste("Mean Slope (weighted by $N$):", format(round(weighted.mean(data$Value, data$n), 4), nsmall = 4), units[i]))
    )
  
  # save plots
  # ggsave(paste0("images/OA_trends/surf_", qty[i], "_by_station.png"), bg = "white")
}

sig_only <- results |> filter(sigp == "Yes") |>
  group_by(station) |> 
  summarize(n_signif = n(),
            lat = mean(lat),
            lon = mean(lon),
            n = mean(n))

ggplot(
  data = world
) +
  geom_sf(fill = "antiquewhite1") +
  geom_point(
    data = sig_only,
    pch =21,
    aes(
      x = lon,
      y = lat,
      size = n_signif,
      fill = n, # number of observations
    ),
    show.legend=TRUE # force shape to always show in legend
  ) +
  geom_text(data = sig_only, nudge_y = -.1 , size = 3, aes(x = lon, y = lat, label= station)) + 
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
    title = "Number of Time Sigificant Variables by Station",
    size = "Number of Time Significant Variables",
    fill = "Number of observations"
  )
ggsave(paste0("images/OA_trends/significant_stations_weighted.png"), bg = "white")



# GENERATE TABULAR SUMMARY ------------------------------------------------

results %>%
  filter(
    n>30
  ) %>%
  group_by(
    qty
  ) %>%
  summarize(
    mean = weighted.mean(Value, n, na.rm = TRUE),
    std = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE),
    n = sum(!is.na(Value))
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
    title = "Summary of By Station Regression Results"
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
  )# %>%
  #gtsave(
  #  "images/OA_trends/lm_by_station_tab.png"
  #)

surf_results %>%
  filter(
    n>15
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
    title = "Summary of By Station Surface Data Regression Results (Depth≤20m)"
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
  )# %>%
  #gtsave(
  #  "images/OA_trends/surf_lm_by_station_tab.png"
  #)


# Analysis 

sig_stations <- results |> 
  filter(sigp == "Yes") |> 
  select(station) |> 
  unique()

non_sig_stations <- results |> 
  filter(sigp == "No") |> 
  select(station) |> 
  unique()

sig_only_p <- results |> filter(sigp == "Yes")

temp_min <- -.07918
temp_max <- -0.03193
ph_min <- -.00255
ph_max <- -.00079
co3_min <- -1.34579
co3_max <- -.59224
calcite_min <- -0.03342
calcite_max <- -0.01347
aragonite_min <- -0.02187
aragonite_max <- -0.00891
dic_min <- .72791
dic_max <- 1.48040
pCO2_min <- .64817
pCO2_max <- 2.14435
Revelle_min <- .03521
revelle_max <- .08289

sig_only_p <- results |> filter(sigp == "Yes") |> 
  mutate(outlier = case_when(
    qty == "T_degC" & (Value < temp_min | Value > temp_max) ~ "Yes",
    qty == "Salnty" ~ "Yes",
    qty == "TA" ~ "Yes",
    qty == "pHin" & (Value < ph_min | Value > ph_max) ~ "Yes",
    qty == "DIC" & (Value < dic_min | Value > dic_max) ~ "Yes",
    qty == "RFin" & (Value < Revelle_min | Value > revelle_max) ~ "Yes",
    qty == "CO3in" & (Value < co3_min | Value > co3_max) ~ "Yes",
    qty == "pCO2in" & (Value < pCO2_min | Value > pCO2_max) ~ "Yes",
    qty == "OmegaCAin" & (Value < calcite_min | Value > calcite_max) ~ "Yes",
    qty == "OmegaARin" & (Value < aragonite_min | Value > aragonite_max) ~ "Yes",
    TRUE ~ "No" 
  ))

sig_only_outlier <- sig_only_p |> filter(outlier == "Yes")

sig_only_outlier_subs <-sig_only_outlier |> select(station, qty, Value) |> filter(station == "090.0 090.0")
