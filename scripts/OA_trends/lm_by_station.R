# lm_by_station.R

library(tidyverse)
library(gt)
library(sf)
library(rnaturalearth)
library(scales)
library(latex2exp)

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

# Get the names of all CalCOFI stations in the data and their locations
stations <- bottle_co2sys %>%
  group_by(
    Station_ID
  ) %>%
  summarize(
    lat = mean(Latitude),
    lon = mean(Longitude),
  )

# Fitting
results <- NULL
for (i in 1:nrow(stations)) {
  data <- bottle_co2sys %>% filter(Station_ID == stations$Station_ID[i])
  for (j in qty) {
    if (data %>% select(paste0(j,"_dtd")) %>% is.na() %>% `!`() %>% sum() == 0) {
      results <- bind_rows(results, c(
        station = stations$Station_ID[i],
        lat = stations$lat[i],
        lon = stations$lon[i],
        qty = j, 
        c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA), 
        n = NA, 
        r2 = NA))
    }
    else {
      fit <- lm(as.formula(paste(paste0(j,"_dtd"),"~","Date_Dec + Depth_Trans")), data = data)
      results <- bind_rows(results, c(
        station = stations$Station_ID[i],
        lat = stations$lat[i],
        lon = stations$lon[i], 
        qty = j, 
        if(nrow(coef(summary(fit))) == 1) c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA) else coef(summary(fit))[2,], 
        n = summary(fit)$df[2] + 2, 
        r2 = summary(fit)$r.squared))
    }
  }
}

results <- results %>%
  mutate(
    across(-c(station, qty), as.numeric)
  ) %>%
  mutate(
    sigp = factor(ifelse(`Pr(>|t|)` < 0.5, 1, 0), levels = c(1,0), labels = c("Yes", "No"))
  )

results %>%
  filter(
    n > 2
  ) %>%
  group_by(
    qty
  ) %>%
  summarize(
    est = mean(Estimate, na.rm = TRUE),
    std = sd(Estimate, na.rm = TRUE),
    n = sum(!is.na(Estimate))
  )

world <- ne_countries(scale = "medium", returnclass = "sf")

qty_names <- c("Temperature", "Salinity", "TA", "DIC", "pCO2", "Revelle Factor", "pH", "CO3$^{2-}$", "$\\Omega_{calcite}$", "$\\Omega_{aragonite}$")
units <- c("$^\\circ$C yr$^{-1}$", "yr$^{-1}$", "$\\mu$mol kg$^{-1}$ yr${^-1}$", 
           "$\\mu$mol kg$^{-1}$ yr${^-1}$", "$\\mu$atm yr$^{-1}$","yr$^{-1}$", 
           "yr$^{-1}$", "$\\mu$mol kg$^{-1}$ yr${^-1}$", "yr$^{-1}$", "yr$^{-1}$")
for (i in 1:10) {
  data <- results %>%
    filter(
      qty == qty[i]
    ) %>%
    filter(
      (!is.na(Estimate)) & (n > 30)
    )
  ggplot(
    data = world
  ) +
    geom_sf(fill = "antiquewhite1") +
    geom_point(
      data = data,
      aes(
        x = lon,
        y = lat,
        fill = Estimate,
        size = n,
        shape = sigp
      ),
      color = "black",
      show.legend=TRUE
    ) +
    coord_sf(
      xlim = c(results$lon %>% min() - 2, results$lon %>% max() + 2),
      ylim = c(results$lat %>% min(), results$lat %>% max())
    ) +
    scale_fill_gradientn(
      colors = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
      values = c(0, 
                 (abs(min(data$Estimate))-abs(quantile(data$Estimate, 0.25)))/(abs(max(data$Estimate)) + (abs(min(data$Estimate)))),
                 abs(min(data$Estimate))/(abs(max(data$Estimate)) + (abs(min(data$Estimate)))), 
                 (abs(min(data$Estimate))+abs(quantile(data$Estimate, 0.75)))/(abs(max(data$Estimate)) + (abs(min(data$Estimate)))),
                 1)
    ) +
    scale_shape_manual(
      values = c("Yes" = 24, "No" = 21),
      drop = FALSE
    ) +
    theme(
      panel.grid.major = element_line(
        color = gray(0.5), 
        linetype = "solid", 
        linewidth = 0.5
      ), 
      panel.background = element_rect(fill = "aliceblue")
    ) +
    guides(
      fill = guide_colorbar(order = 1),
      size = guide_legend(order = 50),
      shape = guide_legend(order = 98)
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = TeX(paste("Estimated Slope for", qty_names[i], "by Station (N>30)")),
      color = "Estimate",
      size = "N",
      shape = TeX("$p<0.5$"),
      caption = TeX(paste("Mean Slope (weighted by $N$):", format(round(weighted.mean(data$Estimate, data$n), 4), nsmall = 4), units[i]))
    )
  ggsave(paste0("images/OA_trends/", qty[i], "_by_station.png"), bg = "white")
}
