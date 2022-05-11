library(tidyverse)
source("R/reliability_functions.R")

set.seed(100)

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location == "US",
    model %in% c("COVIDhub-baseline", "COVIDhub-ensemble", "KITmetricslab-select_ensemble"),
    quantile %in% c(0.25, 0.5, 0.75)
  ) %>%
  rename(qlevel = quantile)

df_reldiag <- df %>%
  group_by(model, qlevel) %>%
  summarize(reldiag(value, truth, alpha = unique(qlevel), n_resamples = 999, digits = 1), .groups = "keep") %>%
  mutate(across(c(x_rc, lower, upper), ~ pmax(., 0))) # set negative values to zero


plot_reldiag(df_reldiag) +
  facet_grid(qlevel ~ model) +
  coord_fixed(ratio = 1, xlim = c(0, 17500), ylim = c(0, 17500))

# ggsave("figures/4_national_reliability.pdf", width = 160, height = 160, unit = "mm", device = "pdf")
