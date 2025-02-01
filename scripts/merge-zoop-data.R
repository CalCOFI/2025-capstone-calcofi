# Access the zooplankton data
library(readr)
calcofi_zoop_data <- read_csv("~/Documents/PSTAT197B/data/195101-201607_1701-1704_1802-1804_Zoop.csv")

# merge the zoop data with the occan acdification data
merged_zoop_data <- inner_join(
  oah_bottle, 
  calcofi_zoop_data,
  by = join_by(Date == Tow_Date, Station_ID == Sta_ID)
)

# Save merged data
write_csv(merged_zoop_data, "data/merged_bottle_data.csv")
