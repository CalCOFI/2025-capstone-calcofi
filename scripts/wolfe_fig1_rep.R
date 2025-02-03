# wolfe_fig1_rep.R

### REPLICATION OF FIG. 1 FROM WOLFE ET. AL 2021 USING COMBINED BOTTLE
### DATASET AND CO2SYS OUTPUT

library(tidyverse)

# read in combined bottle data
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")

# read in CO2SYS output (based on inputs from merged bottle dataset)
co2sys_out <- read_csv("data/CO2SYS_out.csv")

# combine combined bottle and CO2SYS output dataframes
co2sys_bottle_data <- bind_cols(merged_bottle_data, co2sys_out)

# figure (a), temperature
proc_bottle_data %>%
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
proc_bottle_data %>%
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