# wolfe_table1_rep.R

# Replicate Table 1 (Regression statistics for detrended data) from Wolfe et. al.

library(tidyverse)
library(lme4)
library(lmerTest)
library(katex)
library(gt)

# Load seasonal detrending function
source("scripts/OA_trends/detrend_data.R")

# Load merged bottle data and CO2SYS output
merged_bottle_data <- read_csv("data/merged_bottle_data.csv")
co2sys_out <- read_csv("data/CO2SYS_out.csv")

# Combine merged bottle data and CO2SYS output and filter out anomalies
bottle_co2sys <- bind_cols(merged_bottle_data, co2sys_out) %>%
  filter(
    Salnty > 30
  )

# Filter for Station 90.0 90.0 observations at surface depths
STA90.90 <- bottle_co2sys %>%
  filter(
    (Station_ID == "090.0 090.0") & (Depth <= 20)
  )

# Create vector of variables to be detrended
qty <- c("T_degC","Salnty","TA","DIC","pCO2in","RFin","pHin","CO3in","OmegaCAin","OmegaARin")

# Detrend variables of interest
STA90.90 <- sea_dtd_data(qty, STA90.90, "Date.cc")

# Plot detrended and original observations
for (i in qty) {
  print(STA90.90 %>%
          ggplot(
            aes(
              x = Date_Dec
            )
          ) +
          geom_point(
            aes(
              y = get(i),
              col = "Original",
              shape = "Original"
            ),
            na.rm = TRUE
          ) +
          geom_point(
            aes(
              y = get(paste0(i, "_dtd")),
              col = "Detrended",
              shape = "Detrended"
            ),
            na.rm = TRUE
          ) +
          scale_color_manual(
            values = c("blue", "red")
          ) +
          scale_shape_manual(
            values = c(16, 4)
          ) +
          theme_bw() +
          guides(
          ) +
          labs(
            x = "Date",
            y = i,
            col = "",
            shape = '',
            title = "Seasonally Detrended Data for Station 90.90 (Depth ≤ 20m)"
          ))
}

# Fit linear models
models <- lapply(
  qty,
  function(x) {
    lm(
      as.formula(paste(paste0(x,"_dtd"),"~","Date_Dec")),
      data = STA90.90
    )
  }
)

# Extract results from fit and convert to table
lapply(
  1:10,
  function(i) {
    c(qty = qty[i], coef(summary(models[[i]]))[2,], n = summary(models[[i]])$df[2] + 2, r2 = summary(models[[i]])$r.squared)
  }
) %>%
  bind_rows() %>%
  mutate(
    across(-qty, as.numeric)
  ) %>%
  mutate(
    qty = c("Temperature", "Salinity", "A~T~", "C~T~", "*p*CO2", "Revelle Factor", "pH", "CO~3~<sup>2-</sup>", "Ω~calcite~", "Ω~aragonite~"),
    units = c("degC yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:mol kg^-1 yr^-1", ":mu:atm yr^-1",
              "yr^-1", "yr^-1", ":mu:mol kg^-1 yr^-1", "yr^-1", "yr^-1")
  ) %>%
  select(
    -`t value`
  ) %>%
  gt(
    rowname_col = "qty"
  ) %>%
  tab_header(
    title = "Regression Statistics for Station 90.90 (Depth ≤ 20m)"
  ) %>%
  tab_row_group(
    label = "Seawater carbonate chemistry",
    rows = c("C~T~", "A~T~", "*p*CO2", "Revelle Factor")
  ) %>%
  tab_row_group(
    label = "Ocean acidification indicators",
    rows = c("pH", "CO~3~<sup>2-</sup>", "Ω~calcite~", "Ω~aragonite~")
  ) %>%
  tab_row_group(
    label = "Hydrography",
    rows = c("Temperature", "Salinity")
  ) %>%
  tab_stubhead(
    label = "Parameter"
  ) %>%
  cols_label(
    Estimate = "Slope",
    `Pr(>|t|)` = "p-value",
    `Std. Error` = "Std. Error",
    units = "Units",
    r2 = md("r<sup>2</sup>")
  ) %>%
  cols_move(
    units,
    after = `Std. Error`
  ) %>%
  fmt_markdown(
    columns = qty
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  fmt_units(
    columns = units
  ) %>%
  fmt_number(
    columns = c("Estimate", "Std. Error", "Pr(>|t|)", "r2"),
    decimals = 4
  ) %>%
  sub_small_vals(
    columns = `Pr(>|t|)`,
    threshold = 0.0001
  ) %>%
  gtsave(
    "images/OA_trends/wolfe_tab1.png"
  )