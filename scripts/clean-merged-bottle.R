### CLEAN MERGED BOTTLE DATA ###

library(tidyverse)

# read in merged bottle data
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")

# drop NaNs and NAs
clean_merged_bottle_data <- merged_bottle_data %>%
  filter(
    !is.nan(TA) & !is.nan(DIC)
  ) %>%
  filter(
    !is.na(SiO3uM) & !is.na(PO4uM) & !is.na(T_degC)
  )
  
# write results to new csv file
write_csv(clean_merged_bottle_data, "data/clean_merged_bottle_data.csv")
  