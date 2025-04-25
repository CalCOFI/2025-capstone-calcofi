# hypothesis_testing.R
library(tidyverse)
library(FDRestimation)

# READ IN DATA ------------------------------------------------------------
source("scripts/ESPER_analysis/ESPER_out_proc.R")
esper_bottle_combined <- esper_out_proc() %>%
  mutate(
    ESPER_input = factor(ESPER_input, levels = c("lim", "all"), ordered = TRUE)
  ) %>%
  filter(
    Salnty > 30
  )

models <- c("Mixed", "LIR", "NN")
inputs <- c("lim", "all")
vars <- c("TA", "DIC")
pval <- c(0)

results_df <- expand.grid(models, inputs, vars, pval) %>%
  `colnames<-`(
    c("model", "input", "var", "pval")
  )

# HYPOTHESIS TESTING ------------------------------------------------------
for (i in 1:nrow(results_df)) {
  results_df$pval[i] <- esper_bottle_combined %>%
    filter(
      ESPER_model == as.character(results_df$model[i]),
      ESPER_input == as.character(results_df$input[i])
    ) %>%
    pull(
      paste0(results_df$var[i],"_res")
    ) %>%
    t.test() %>%
    `$`(
      "p.value"
    )
}

results_df <- results_df %>% 
  mutate(
    adj.pval = (p.fdr(pvalues = .data$pval))$fdrs
  )

results_df %>%
  select(
    -pval 
  ) %>%
  pivot_wider(
    names_from = c(input, var),
    values_from = adj.pval
  ) %>%
  group_by(
    model
  ) %>%
  gt(
    row_group_as_column = TRUE
  ) %>%
  tab_header(
    title = "ESPER Residuals Hypothesis Testing Results (p-values)"
  ) %>%
  tab_spanner(
    label = "TA",
    columns = c(lim_TA, all_TA)
  ) %>%
  tab_spanner(
    label = "DIC",
    columns = c(lim_DIC, all_DIC)
  ) %>%
  tab_stubhead(
    label = "Model"
  ) %>%
  cols_label(
    lim_TA = "lim",
    all_TA = "all",
    lim_DIC = "lim",
    all_DIC = "all"
  ) %>%
  fmt_number(
    columns = c(lim_TA, all_TA, lim_DIC, all_DIC),
    decimals = 4
  ) %>%
  sub_small_vals(
    columns = c(lim_TA, all_TA, lim_DIC, all_DIC),
    threshold = 0.0001
  ) %>%
  opt_stylize(
    style = 3
  ) %>%
  gtsave(
    "images/ESPER_analysis/hypothesis_test_tab.png"
  )