library(tidyverse)
library(ggnewscale)

# Load merged bottle data and CO2SYS output
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")
co2sys_out <- read_csv("data/CO2SYS_out.csv")

# Combine merged bottle data and CO2SYS output and filter out anomalies
bottle_co2sys <- bind_cols(merged_bottle_data, co2sys_out) %>%
  filter(
    Salnty > 30,
    Depth < 1000
  )

mod_surface <- lm(
  pCO2in ~ Date_Dec,
  data = bottle_co2sys %>% filter(Depth <= 20)
)
mod_time <- lm(
  pCO2in ~ Date_Dec,
  data = bottle_co2sys
)
mod_full <- lm(
  pCO2in ~ Date_Dec + Depth,
  data = bottle_co2sys
)
t <- seq(min(bottle_co2sys$Date_Dec), max(bottle_co2sys$Date_Dec), length.out = 10000)

ggplot() +
  geom_point(
    aes(
      y = pCO2in,
      x = Date_Dec,
      color = Depth,
    ),
    alpha=0.8,
    na.rm = TRUE,
    data = bottle_co2sys
  ) +
  new_scale_color() +
  geom_point(
    aes(
      y = pCO2in,
      x = Date_Dec,
      color = factor(ifelse(Depth<=20,1,0),labels=c("Yes")),
    ),
    alpha=0.4,
    na.rm = TRUE,
    shape = 21,
    data = bottle_co2sys %>% filter(Depth <= 20)
  ) +
  scale_color_manual(
    values = c("Yes" = "red")
  ) +
  labs(
    color = "Surface"
  ) +
  new_scale_color() +
  geom_ribbon(
    aes(
      x = t,
      ymin = predict(mod_surface, newdata = data.frame(Date_Dec=t), interval = "confidence")[,2],
      ymax = predict(mod_surface, newdata = data.frame(Date_Dec=t), interval = "confidence")[,3]
    ),
    fill = "gray",
    alpha = 0.8
  ) +
  geom_line(
    aes(
      x = t,
      y = predict(mod_surface, newdata = data.frame(Date_Dec=t)),
      col = "y ~ t (surface)"
    ),
    linewidth = 1.2
  ) +
  geom_ribbon(
    aes(
      x = t,
      ymin = predict(mod_time, newdata = data.frame(Date_Dec=t), interval = "confidence")[,2],
      ymax = predict(mod_time, newdata = data.frame(Date_Dec=t), interval = "confidence")[,3]
    ),
    fill = "gray",
    alpha = 0.8
  ) +
  geom_line(
    aes(
      x = t,
      y = predict(mod_time, newdata = data.frame(Date_Dec=t)),
      col = "y ~ t"
    ),
    linewidth = 1.2
  ) +
  geom_ribbon(
    aes(
      x = t,
      ymin = predict(mod_full, newdata = data.frame(Date_Dec=t,Depth=10), interval = "confidence")[,2],
      ymax = predict(mod_full, newdata = data.frame(Date_Dec=t,Depth=10), interval = "confidence")[,3]
    ),
    fill = "gray",
    alpha = 0.8
  ) +
  geom_line(
    aes(
      x = t,
      y = predict(mod_full, newdata = data.frame(Date_Dec = t, Depth=10)),
      col = "y ~ t + depth"
    ),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c("y ~ t (surface)" = "blue", "y ~ t" = "red", "y ~ t + depth" = "purple")
  ) +
  labs(
    x = "Date",
    color = "Model",
    fill = "Depth",
    caption = "y ~ t + depth fit plotted for depth = 10m"
  ) +
  theme_bw()
