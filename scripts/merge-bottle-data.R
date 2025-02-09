# merge_bottle_data.R

### MERGE CALCOFI OCEANOGRAPHIC AND CARBONATE CHEMISTRY DATA ###

library(tidyverse)

# Read in oceanographic bottle data
hydro_bottle <- read_csv(
  "data/calcofi_hydro_bottle/194903-202105_Bottle.csv",
   # change encoding
   locale=locale(encoding="latin1"),
   # increase guess_max to correctly guess column types
   guess_max = Inf
)

# Read in cast data
cast_bottle <- read_csv("data/calcofi_hydro_bottle/194903-202105_Cast.csv")

# Read in carbonate chemistry bottle data
cc_bottle <- read_csv("data/carbonate_chem_bottle.csv")

# Drop first row (containing units) of carbonate chemistry bottle data
cc_bottle <- cc_bottle[2:nrow(cc_bottle),]

# Merge oceanographic and cast data based on Cst_Cnt (Cast Count) and Sta_ID (Station ID)
hydro_bottle <- hydro_bottle %>%
  left_join(
    cast_bottle,
    by = join_by(Cst_Cnt, Sta_ID)
  )

# Prepare hydro bottle data for merging
depth_tol <- 0.9
hydro_bottle <- hydro_bottle %>%
  mutate(
    Date = as.Date(Date, format = c("%m/%d/%Y"))
  ) %>%
  mutate(
    Year = year(Date),
    Month = month(Date)
  ) %>%
  mutate(
    Depthm_Upper = Depthm + depth_tol,
    Depthm_Lower = Depthm - depth_tol
  )

# Prepare carbonate chemistry data for merging
cc_bottle <- cc_bottle %>%
  # Create new date column for merging
  mutate(
    Date = as.Date(
      paste(Month_UTC, Day_UTC, Year_UTC, sep = "/"),
      tryFormats = c("%m/%d/%Y")
    ),
    .before = Year_UTC
  ) %>%
  # Change column types for merging
  mutate(
    Depth = as.double(Depth),
    Latitude = as.double(Latitude),
    Longitude = as.double(Longitude)
  )

# Merge carbonate chemistry and oceanographic bottle data based on date, location, and depth
merged_bottle_data <- inner_join(
  cc_bottle, 
  hydro_bottle,
  by = join_by(Month_UTC == Month,
               Year_UTC == Year,
               Station_ID == Sta_ID,
               between(Depth, Depthm_Lower, Depthm_Upper)),
)

# Save merged data
write_csv(merged_bottle_data, "data/merged_bottle_data.csv")