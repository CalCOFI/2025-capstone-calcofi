# Access the zooplankton data
library(readr)
calcofi_zoop_data <- read_csv("~/Documents/PSTAT197B/data/195101-201607_1701-1704_1802-1804_Zoop.csv")

# change the date format in oah_bottle

oah_bottle_format <- oah_bottle %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

calcofi_zoop_data <- calcofi_zoop_data %>% 
  mutate(Tow_Date = as.Date(Tow_Date, format = "%m/%d/%Y")) 

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

# Load necessary libraries
library(dplyr)     # For data manipulation
library(ggplot2)   # For visualization
library(naniar)    # For advanced missing data analysis
library(VIM)       # For missing value visualization
library(visdat)    # For quick visualization
library(tidyverse) # General data handling

# Basic Missing Value Summary
missing_zoop_summary <- colSums(is.na(merged_zoop_data))
missing_zoop_df <- data.frame(Variable = names(missing_zoop_summary), Missing_Count = missing_zoop_summary)

# Percentage of Missing Values per Column
missing_zoop_percent <- missing_zoop_df %>%
  mutate(Percent_Missing = (Missing_Count / nrow(merged_zoop_data)) * 100) %>%
  arrange(desc(Percent_Missing))  

print(missing_zoop_percent)

# Visualize Missing Data Pattern
# Calculate the missing rate for each column
missing_rate <- colSums(is.na(merged_zoop_data)) / nrow(merged_zoop_data)

# Select only columns where missing rate is > 0 and < 1 (partially missing)
zoop_missing_cols <- merged_zoop_data %>%
  select(which(missing_rate >= 0 & missing_rate < 1))

vis_miss(zoop_missing_cols)  

ggplot(missing_zoop_percent, aes(x = reorder(Variable, -Percent_Missing), y = Percent_Missing)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Percentage of Missing Data per Variable",
       x = "Variable",
       y = "Percentage Missing") +
  theme_minimal()
