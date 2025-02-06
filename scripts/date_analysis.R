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

# convert dates to common format
cc_bottle <- cc_bottle %>%
  mutate(
    Date = as.Date(
      paste(Month_UTC, Day_UTC, Year_UTC, sep = "/"),
      tryFormats = c("%m/%d/%Y")
    ),
    .before = Year_UTC
  ) %>%
  # Change column types for merging
  mutate(
    Depth = as.double(Depth)
  )

hydro_bottle <- hydro_bottle %>%
  mutate(
    Date = as.Date(Date, format = c("%m/%d/%Y"))
  )

# generate histogram of days for carb chem data
cc_bottle %>%
  select(
    Day_UTC
  ) %>%
  ggplot(
    aes(
      x = Day_UTC
    )
  ) +
  geom_histogram(bins = 31)

# generate histogram of days for hydrographic data
hydro_bottle %>%
  select(
    Date
  ) %>%
  mutate(
    day = day(Date)
  ) %>%
  ggplot(
    aes(
      x = day
    )
  ) +
  geom_histogram(
    bins = 31
  )

# get unmatched dates
unmatched_dates <- setdiff(cc_bottle$Date, intersect(cc_bottle$Date, hydro_bottle$Date)) %>% as_date()

# tabulate observations with unmatched dates
cc_bottle %>%
  filter(
    Date %in% unmatched_dates
  ) %>%
  select(
    Year_UTC, Month_UTC, Day_UTC
  ) %>%
  table() %>%
  addmargins() %>%
  `[`(,,1) %>%
  formattable::formattable()

# plot of observations with unmatched dates by year, month, day
cc_bottle %>%
  filter(
    Date %in% unmatched_dates
  ) %>%
  group_by(
    Date
  ) %>%
  summarize(
    count = n(),
    Month_UTC = first(Month_UTC),
    Day_UTC = first(Day_UTC),
    Year_UTC = first(Year_UTC)
  ) %>%
  ggplot(
    aes(
      x = factor(Year_UTC),
      y = factor(Month_UTC, levels = 1:12, labels = month.abb),
      size = count
    )
  ) +
  geom_point() +
  facet_wrap(
    vars(Day_UTC)
  ) +
  scale_y_discrete(
    limits = month.abb
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45)
  ) + 
  labs(
    x = "Year",
    y = "Month",
    size = "Count"
  )