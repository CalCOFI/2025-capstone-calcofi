library(readr)
library(dplyr)
library(lubridate)
zoop_new <- read_csv("Documents/PSTAT197B/capstone-calcofi-seagrant/data/zoop_data/Zooplankton-new.csv")
# change the data format of date and station id
zoop_new <- zoop_new %>% 
  mutate(
    time = ymd_hms(time),  # Convert to proper Date-Time format
    Date = as.Date(time),  # Extract Date
    Time = format(time, "%H:%M:%S")  # Extract Time as character
  ) %>%
  select(-time)  # Remove original datetime column if necessary

zoop_new <- zoop_new %>%
  mutate(
    Year_UTC = year(Date),
    Month_UTC = month(Date),
    Day_UTC = day(Date)
  )

zoop_new$Station_ID <- paste(
  sprintf('%05.1f', zoop_new$line),
  sprintf('%05.1f', as.numeric(zoop_new$station)),
  sep = ' '
)
# treat the cc_bottle data to make sure the measurements are numeric
merged_bottle_co2sys <- merged_bottle_co2sys %>% mutate(DIC = as.numeric(DIC),
                                  TA = as.numeric(TA),
                                  Depth = as.numeric(Depth),
                                  CTDTEMP_ITS90 = as.numeric(CTDTEMP_ITS90),
                                  Salinity_PSS78 = as.numeric(Salinity_PSS78),
                                  Longitude = as.numeric(Longitude),
                                  Latitude = as.numeric(Latitude)
                                  )

# count the cc_bottle/zoop data 
obs_count <-  merged_bottle_co2sys %>% count(Year_UTC, Month_UTC, Station_ID) 
average_observations <- mean(obs_count$n)
# on average 4.12 observations of carbochem(dic) data per month per station id
obs_count <-  zoop_new %>% count(Year_UTC, Month_UTC, Station_ID) 
average_observations <- mean(obs_count$n)
# on average 1.02 observations of zoop data per month per station id 

# average the cc_bottle data per date per location
num_cc<- merged_bottle_co2sys[,c("Date_cc", "Year_UTC", "Month_UTC", "Day_UTC", "Station_ID", 
                      "Latitude", "Longitude", "TA", "DIC", "Depth", 
                      "CTDTEMP_ITS90", "Salinity_PSS78")]
cc_avg_daily <- num_cc %>% group_by(Date_cc, Station_ID) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()
# save the daily averaged data
write_csv(cc_avg_daily, "data/cc_avg_daily.csv")

# try merge it with the zooplankton data 
merged_zoop_avg_daily <- inner_join(
  cc_avg_daily, 
  zoop_new,
  by = join_by(Date_cc == Date, Station_ID == Station_ID)
)


cc_avg_monthly <- num_cc %>% group_by(Year_UTC, Month_UTC, Station_ID) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()
# save the daily averaged data
write_csv(cc_avg_monthly, "data/cc_avg_monthly.csv")

# try merge it with the zooplankton data 
merged_zoop_avg_monthly <- inner_join(
  cc_avg_monthly, 
  zoop_new,
  by = join_by(Year_UTC == Year_UTC, Month_UTC == Month_UTC, Station_ID == Station_ID)
)

# subtract the small plankton from total plankton to get large plankton
merged_zoop_avg_monthly <- merged_zoop_avg_monthly %>% mutate(large_plankton = total_plankton - small_plankton)

write_csv(merged_zoop_avg_monthly, "data/merged_zoop_avg_monthly.csv")
