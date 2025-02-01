# Access the zooplankton data
library(readr)
calcofi_zoop_data <- read_csv("~/Documents/PSTAT197B/data/195101-201607_1701-1704_1802-1804_Zoop.csv")

# change the date format in oah_bottle

oah_bottle_format <- oah_bottle %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(Date = format(Date, "%m/%d/%Y"))

# merge the zoop data with the ocean acidification data
merged_zoop_data <- inner_join(
  oah_bottle_format, 
  calcofi_zoop_data,
  by = join_by(Date == Tow_Date, Station_ID == Sta_ID)
)

# Save merged data
write_csv(merged_zoop_data, "data/merged_zoop_data.csv")

# Merge carbonate chemistry and oceanographic bottle data based on date, location, and depth
merged_bottle_data <- inner_join(
  oah_bottle_format, 
  hydro_bottle,
  by = join_by(Date_format == Date, Depth == Depthm, Station_ID == Sta_ID)
)

# Save merged data
write_csv(merged_bottle_data, "data/merged_bottle_data.csv")
