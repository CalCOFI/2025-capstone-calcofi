# replicate_wolfe_fig1.R

### REPLICATION OF FIG. 1 FROM WOLFE ET. AL 2021 USING COMBINED BOTTLE
### DATASET AND CO2SYS OUTPUT

library(tidyverse)

# read in combined bottle data
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")

# read in CO2SYS output (based on inputs from merged bottle dataset)
co2sys_out <- read_csv("data/CO2SYS_out.csv")

# combine combined bottle and CO2SYS output dataframes
co2sys_bottle_data <- bind_cols(merged_bottle_data, co2sys_out)

# create 3 month sliding scale bins for average seasonal cycles and compute means 
# and standard errors (for depth <= 20m)
binned_co2sys_bottle <- lapply(
  1:12,
  function(i) {
    co2sys_bottle_data %>%
      filter(
        Month_UTC%%12 %in% (c(i-1,i,i+1)%%12)
      ) %>%
      mutate(
        binned_month = i
      )
  }
) %>%
  bind_rows() %>%
  filter(
    Depth <= 20
  ) %>%
  group_by(
    binned_month
  ) %>%
  summarize(
    T_degC.mu = mean(T_degC, na.rm = TRUE),
    T_degC.serr = sd(T_degC, na.rm = TRUE)/sqrt(sum(!is.na(T_degC))),
    Salnty.mu = mean(Salnty, na.rm = TRUE),
    Salnty.serr = sd(Salnty, na.rm = TRUE)/sqrt(sum(!is.na(Salnty))),
    TAlk.mu = mean(TAlk, na.rm = TRUE),
    TAlk.serr = sd(TAlk,na.rm = TRUE)/sqrt(sum(!is.na(TAlk))),
    TCO2.mu = mean(TCO2, na.rm = TRUE),
    TCO2.serr = sd(TCO2, na.rm = TRUE)/sqrt(sum(!is.na(TCO2))),
    pCO2in.mu = mean(pCO2in, na.rm = TRUE),
    pCO2in.serr = sd(pCO2in, na.rm = TRUE)/sqrt(sum(!is.na(pCO2in))),
    RFin.mu = mean(RFin, na.rm = TRUE),
    RFin.serr = sd(RFin, na.rm = TRUE)/sqrt(sum(!is.na(RFin))),
    pHin.mu = mean(pHin, na.rm = TRUE),
    pHin.serr = sd(pHin, na.rm = TRUE)/sqrt(sum(!is.na(pHin))),
    CO3in.mu = mean(CO3in, na.rm = TRUE),
    CO3in.serr = sd(CO3in, na.rm = TRUE)/sqrt(sum(!is.na(CO3in))),
    OmegaCAin.mu = mean(OmegaCAin, na.rm = TRUE),
    OmegaCAin.serr = sd(OmegaCAin, na.rm = TRUE)/sqrt(sum(!is.na(OmegaCAin))),
    OmegaARin.mu = mean(OmegaARin, na.rm = TRUE),
    OmegaARin.serr = sd(OmegaARin, na.rm = TRUE)/sqrt(sum(!is.na(OmegaARin)))
  )

# figure (a), temperature
co2sys_bottle_data %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date,
      y = T_degC,
    )
  ) +
  geom_point(na.rm = TRUE) +
  labs(
    y = "Temperature (Cº)"
  ) +
  theme_minimal()

# figure (a), Salinity
co2sys_bottle_data %>%
  filter(
    Depth <= 20
  ) %>%
  ggplot(
    aes(
      x = Date,
      y = Salnty,
    )
  ) +
  geom_point(na.rm = TRUE) +
  labs(
    y = "Salinity"
  ) +
  theme_minimal()

# figure (g), temperature

proc_bottle_data %>%
  filter(
    Depth <= 100
  ) %>%
  ggplot(
    aes(
      x = Date,
      y = TA,
      col = Depth
    )
  ) +
  geom_point(na.rm = TRUE) +
  labs(
    col = "Depth (m)"
  ) +
  theme_minimal()

proc_bottle_data %>%
  filter(
    Depth <= 100
  ) %>%
  ggplot(
    aes(
      x = Date,
      y = TCO2,
      col = Depth
    )
  ) +
  geom_point(na.rm = TRUE) +
  labs(
    col = "Depth (m)"
  ) +
  theme_minimal()

proc_bottle_data %>%
  filter(
    Depth <= 100
  ) %>%
  ggplot(
    aes(
      x = Date,
      y = pCO2in,
      col = Depth
    )
  ) +
  geom_point(na.rm = TRUE) +
  labs(
    col = "Depth (m)"
  ) +
  theme_minimal()

proc_bottle_data %>%
  filter(
    Depth <= 100
  ) %>%
  ggplot(
    aes(
      x = Date,
      y = pHin,
      col = Depth
    )
  ) +
  geom_point(na.rm = TRUE) +
  labs(
    col = "Depth (m)"
  ) +
  theme_minimal()

proc_bottle_data %>%
  filter(
    Depth <= 100
  ) %>%
  ggplot(
    aes(
      x = Date,
      y = OmegaCAin,
      col = Depth
    )
  ) +
  geom_point(na.rm = TRUE) +
  labs(
    col = "Depth (m)"
  ) +
  theme_minimal()