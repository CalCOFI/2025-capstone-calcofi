library(tidyverse)
library(ggforce)
library(scales)
library(ModelMetrics)

### Correct Anomalous Salnty Values ###
merged_bottle_data <- read_csv(here::here("data/merged_bottle_data.csv"))

merged_bottle_anom <- merged_bottle_data |> 
  mutate(anom = case_when(
    Salnty < 30 ~ TRUE,
    TRUE ~ FALSE)) |> 
  filter(anom == T)

write_csv(merged_bottle_anom, file = here::here("data/carbon_chem_anomalies.csv"))