# trends_by_depth.R

library(tidyverse)


# LOAD DATA ---------------------------------------------------------------

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

# Create vector of variables to be fit
qty <- c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin")

# Manually determine depth bins
breaks <- c(0,20,40,70,130,200,260,400,572)
bottle_co2sys$Depth %>% table() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  `colnames<-`(c("depth","freq")) %>%
  mutate(
    across(everything(),as.numeric)
  ) %>%
  mutate(
    label = ifelse(freq > 100, paste0(depth,"m"), NA)
  ) %>%
  ggplot() +
  geom_histogram(
    aes(
      x = Depth
    ),
    data = bottle_co2sys,
    breaks = breaks,
    alpha = 0.2,
    fill = "#56B1F7"
  ) +
  geom_vline(
    xintercept = breaks,
    linetype = 2,
    alpha = 0.4
  ) +
  geom_point(
    aes(
      x = depth,
      y = freq,
      size = freq,
      color = freq
    )
  ) +
  geom_text(
    aes(
      x = depth,
      y = freq,
      label = label
    ),
    na.rm = TRUE,
    nudge_y=30
  ) +
  theme_bw() +
  guides(
    color = "legend",
    size = "legend"
  ) +
  labs(
    x = "Depth (m)",
    y = "Count",
    color = "Count",
    size = "Count"
  )

# Create depth bins
labels <- paste(c(breaks[1],breaks[-c(1,length(breaks))]+1),"-",breaks[-1],"m")
bottle_co2sys <- bottle_co2sys %>%
  mutate(
    depth_bin = cut(Depth, breaks=breaks, labels=labels, include.lowest = TRUE)
  )

# Extract stations exceeding minimum number of observations in all depth bins
min_unique <- 7 # number of unique years
min_n <- 20 # number of observations
stations <- bottle_co2sys %>%
  filter(
    if_all(c(qty, c(Depth, Date.cc)), ~!is.na(.x))
  ) %>%
  group_by(
    Station_ID, St_Line, St_Station, depth_bin
  ) %>%
  summarize(
    length = max(Year_UTC) - min(Year_UTC) + 1,
    n = n(),
    unique = length(unique(Year_UTC)),
    lat = mean(Latitude),
    lon = mean(Longitude),
  ) %>%
  ungroup() %>%
  filter(
    unique >= min_unique,
    n >= min_n,
  ) %>%
  group_by(
    Station_ID
  ) %>%
  mutate(
    num_bins = length(unique(depth_bin))
  ) %>%
  filter(
    num_bins == length(breaks) - 1
  )

# tabulate observations in each bin by station
stations %>%
  select(
    Station_ID, depth_bin, n
  ) %>%
  pivot_wider(
    names_from = Station_ID,
    values_from = n
  ) %>%
  gt(
    rowname_col = "depth_bin"
  ) %>%
  tab_header(
    title = "Number of Observations by Depth at the Most Sampled CalCOFI Stations"
  ) %>%
  grand_summary_rows(
    fns = list(Total ~ sum(.))
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  gtsave(
    "images/OA_trends/most_sampled_obs_by_depth.png"
  )

# PERFORM FITS ------------------------------------------------------------

num_stations <- length(unique(stations$Station_ID)); print(num_stations)
num_bins <- length(breaks) - 1; print(num_bins)

# create fits and results object
fits <- NULL
results <- NULL

for (i in 1:nrow(stations)) {
  data <- bottle_co2sys %>% 
    # extract data for the station
    filter(
      Station_ID == stations$Station_ID[i]
    ) %>%
    # and the specific depth range
    filter(
      depth_bin == stations$depth_bin[i]
    ) %>%
    # and detrend each variable of interest
    sea_dtd_data(qty = qty, df = ., date_col = "Date.cc")
  
  for (j in 1:length(qty)) {
    # fit the linear model
    fit <- lm(
      as.formula(paste0(qty[j],"_dtd ~ Date_Dec")), 
      data = data, 
      na.action = na.exclude
    )
    
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
        st_line = stations$St_Line[i],
        st_station = stations$St_Station[i],
        depth_bin = stations$depth_bin[i],
        if(nrow(coef(summary(fit))) == 1) c(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA) else coef(summary(fit))[2,], 
        n = summary(fit)$df[2] + 2
      )
    )
  }
}


# PLOT RESULTS ------------------------------------------------------------

# create vector of (full) names for each quantity
qty_names <- c("Temperature", "Salinity", "TA", "DIC", "pCO2", "Revelle Factor", "pH", "CO3$^{2-}$", "$\\Omega_{calcite}$", "$\\Omega_{aragonite}$")

# create vector of units for each quantity
units <- c("$^\\circ$C yr$^{-1}$", "yr$^{-1}$", "µmol kg$^{-1}$ yr$^{-1}$", 
           "µmol kg$^{-1}$ yr$^{-1}$", "µatm yr$^{-1}$","yr$^{-1}$", 
           "yr$^{-1}$", "µmol kg$^{-1}$ yr$^{-1}$", "yr$^{-1}$", "yr$^{-1}$")

# modify results objects for plotting
results <- results %>%
  # convert numeric columns to numeric vectors
  mutate(
    across(-c(station, qty), as.numeric)
  ) %>%
  # implement multiple testing correction
  mutate(
    adj.pval = (p.fdr(pvalues = .data$`Pr(>|t|)`))$fdrs
  ) %>%
  # create vector indicating if p < 0.05
  mutate(
    sigp = factor(ifelse(`Pr(>|t|)` < 0.05, 1, 0), levels = c(1,0), labels = c("Yes", "No")),
    adj.sigp = factor(ifelse(adj.pval < 0.05, 1, 0), levels = c(1,0), labels = c("Yes", "No"))
  )

# generate vector of plotting depths
x <- breaks[-length(breaks)] + diff(breaks)/2

# plot estimates against depth for each quantity
for (i in 1:length(qty)) {
  print(
    results %>%
      filter(
        qty == qty[i]
      ) %>%
      mutate(
        plot_depth = x[as.integer(depth_bin)]
      ) %>%
      ggplot(
        aes(
          x = plot_depth,
          shape = adj.sigp,
          color = adj.sigp
        )
      ) +
      geom_hline(
        yintercept = 0,
        linetype = 3
      ) +
      geom_point(
        aes(
          y = Estimate,
        )
      ) +
      geom_errorbar(
        aes(
          ymin = Estimate - `Std. Error`,
          ymax = Estimate + `Std. Error`
        )
      ) +
      facet_wrap(
        vars(station)
      ) +
      theme_bw() +
      scale_color_manual(
        values = c("Yes" = "red", "No" = "blue")
      ) +
      scale_shape_manual(
        values = c("Yes" = 17, "No" = 16)
      ) +
      guides(
        shape = "legend",
        color = "legend"
      ) +
      labs(
        x = "Depth (m)",
        y = TeX(paste0("Estimated Slope (", units[i],")")),
        title = TeX(paste0("Estimated Slope for ",qty_names[i]," against Depth")),
        shape = "Significance",
        color = "Significance"
      )
  )
  
  # save plot
  ggsave(paste0("images/OA_trends/",qty[i],"_by_depth.png"), bg = "white")
}
