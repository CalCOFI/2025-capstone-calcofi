# hypothesis_testing.R

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


# HYPOTHESIS TESTING ------------------------------------------------------
for (i in 1:6) {
  print(models[i%%3+1])
  print(inputs[i%%2+1])
  esper_bottle_combined %>%
    filter(
      ESPER_model == models[i%%3+1], ESPER_input == inputs[i%%2+1]
    ) %>%
    pull(
      TA_res
    ) %>%
    t.test() %>%
    print()
}