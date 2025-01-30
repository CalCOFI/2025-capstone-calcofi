library(tidyverse)

# Read in oceanographic data
hydro_bottle <- read_csv(
  "calcofi_hydro_bottle/194903-202105_Bottle.csv",
   # change encoding
   locale=locale(encoding="latin1"),
   # increase guess_max to correctly guess column types
   guess_max = Inf
)

# Read in cast data
cast_bottle <- read_csv("calcofi_hydro_bottle/194903-202105_Cast.csv")

# Read in ocean acidification bottle data
oah_bottle <- read_csv("calcofi_oah_bottle.csv")

# Drop first row (units) of ocean acidification bottle data
oah_bottle <- oah_bottle[2:nrow(oah_bottle),]

# Merge oceanographic and cast data based on Cst_Cnt (Cast Count) and Sta_ID (Station ID)
hydro_bottle <- hydro_bottle %>%
  left_join(
    cast_bottle,
    by = join_by(Cst_Cnt, Sta_ID)
  )

oah_bottle <- oah_bottle %>%
  # Create new date column for merging
  mutate(
    Date = paste(Month_UTC, Day_UTC, Year_UTC, sep = "/"),
    .before = Year_UTC
  ) %>%
  # Change column types for merging
  mutate(
    Latitude = as.double(Latitude),
    Longitude = as.double(Longitude),
    Depth = as.double(Depth)
  )

# Merge ocean acidification and oceanographic bottle data based on date, location, and depth
merged_bottle_data <- left_join(
  oah_bottle, 
  hydro_bottle,
  by = join_by(Date, Latitude == Lat_Dec, Longitude == Lon_Dec, Depth == Depthm, Station_ID == Sta_ID)
  )

# Save merged data
write_csv(merged_bottle_data, "data/merged_bottle_data.csv")