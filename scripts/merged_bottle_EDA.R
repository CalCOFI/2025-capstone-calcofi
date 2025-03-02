# merged_bottle_EDA.R

### EDA ON MERGED BOTTLE DATASET ###

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(ggforce)
library(scales)
library(visdat)

# read in merged bottle data
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")

# summary of year and depth
merged_bottle_data[,c("Year_UTC","Depth")] %>%
  summary()

# number of unique stations
merged_bottle_data$Station_ID %>% unique() %>% length()

# visualize missingnesss
vis_miss(merged_bottle_data[,c("Year_UTC","Month_UTC","Depth","TA","DIC","T_degC","Salnty","SiO3uM","PO4uM")])
ggsave("images/merged_bottle_EDA/merged_bottle_missingness.png", bg = "white")

# create plot of number of observations against year
merged_bottle_data %>%
  group_by(
    Year_UTC
  ) %>%
  summarize(
    N = n()
  ) %>%
  add_row(
    Year_UTC = 2002:2007, N = 0
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
ggsave("images/merged_bottle_EDA/obs_by_year.png", bg = "white")

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
  scale_y_reverse() +
  scale_y_continuous(
    transform = trans_reverser("pseudo_log"),
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
ggsave("images/merged_bottle_EDA/depth_vs_yr.png", bg = "white")

# plot map of stations and number of observations at stations
world <- ne_countries(scale = "medium", returnclass = "sf")
# states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

ggplot(
  data = world
) +
  geom_sf(fill = "antiquewhite1") +
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
  coord_sf(
    xlim = c(merged_bottle_data$Longitude %>% min() - 2, merged_bottle_data$Longitude %>% max() + 2),
    ylim = c(merged_bottle_data$Latitude %>% min() - 1,merged_bottle_data$Latitude %>% max() + 1)
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5), 
      linetype = "solid", 
      linewidth = 0.5
    ), 
    panel.background = element_rect(fill = "aliceblue")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Number of Observations by Station in Merged Bottle Dataset",
    size = "Observations"
  )
ggsave("images/merged_bottle_EDA/obs_by_station.png", bg = "white")