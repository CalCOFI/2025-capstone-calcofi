# merged_bottle_EDA.R

### EDA ON MERGED BOTTLE DATASET ###

library(tidyverse)
library(maps)

# read in merged bottle data
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")

# summary of year and depth
merged_bottle_data[,c("Year_UTC","Depth")] %>%
  summary()

# number of unique stations
merged_bottle_data$Station_ID %>% unique() %>% length()

# create plot of number of observations against year
merged_bottle_data %>%
  group_by(
    Year_UTC
  ) %>%
  summarize(
    N = n()
  ) %>%
  ggplot(
    aes(
      x = as.factor(Year_UTC),
      y = N
    )
  ) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Number of Observations",
    title = "Number of Observations by Year in Merged Bottle Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45)
  )
ggsave("images/merged_bottle_EDA/obs_by_year.png")

# create plot of depth against year, with size and color indicating number of observations
merged_bottle_data %>%
  group_by(
    Year_UTC, Depth
  ) %>%
  summarize(
    Count = n()
  ) %>%
  ggplot(
    aes(
      x = Year_UTC,
      y = Depth,
      size = Count,
      col = Count
    )
  ) +
  geom_point() +
  scale_y_continuous(
    transform = pseudo_log_trans(base = 10),
    breaks = c(1,10,100,1000)
  ) +
  scale_size_continuous(limits=c(1, 75), breaks=seq(0,75, by=20)) +
  guides(col=guide_legend(), size=guide_legend()) +
  scale_color_continuous(limits=c(1, 75), breaks=seq(0,75, by=20)) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Depth (m)",
    col = "Observations",
    size = "Observations",
    title = "Depth vs. Year in Merged Bottle Dataset"
  )
ggsave("images/merged_bottle_EDA/depth_vs_yr.png")

# plot map of stations and number of observations at stations
world <- map_data("world")
ca_counties <- subset(map_data("county"), region == "california")

ggplot() +
  geom_polygon(
    data = world,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "black",
    fill = "gray90"
  ) +
  geom_point(
    data = merged_bottle_data %>%
      group_by(
        Station_ID
      ) %>%
      summarize(
        Latitude = first(Latitude),
        Longitude = first(Longitude),
        Count = n()
      ),
    aes(
      x = Longitude,
      y = Latitude,
      size = Count
    )
  ) +
  coord_cartesian(
    xlim = c(merged_bottle_data$Longitude %>% min() - 1.5, merged_bottle_data$Longitude %>% max() + 3.5),
    ylim = c(merged_bottle_data$Latitude %>% min() - 1,merged_bottle_data$Latitude %>% max() + 1)
  ) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Number of Observations by Station in Merged Bottle Dataset",
    size = "Observations"
  )
ggsave("images/merged_bottle_EDA/obs_by_station.png")