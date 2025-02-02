### CLEAN MERGED BOTTLE DATA ###

library(tidyverse)

merged_bottle_data <- read_csv("data/merged_bottle_data.csv")

clean_merged_bottle_data <- merged_bottle_data %>%
  filter(
    !is.nan(TA) & !is.nan(DIC)
  ) %>%
  filter(
    !is.na(SiO3uM) & !is.na(PO4uM) & !is.na(T_degC)
  )
  
write_csv(clean_merged_bottle_data, "data/clean_merged_bottle_data.csv")

# EXPOCODE, Ship_Name.x, Date, Latitude, Longitude, Depth, DIC, TA, Salnty, T_degC, SiO3uM, PO4uM

  