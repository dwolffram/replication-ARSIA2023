library(tidyverse)
source("R/reliability_functions.R")

set.seed(100)

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location == "US",
    target == "1 wk ahead inc death",
    model %in% c("COVIDhub-baseline", "COVIDhub-ensemble", "KITmetricslab-select_ensemble"),
    quantile %in% c(0.25, 0.5, 0.75)
  )

df_reldiag <- df %>%
  group_by(model, quantile) %>%
  summarize(reldiag(value, truth, alpha = unique(quantile), n_resamples = 999, digits = 1), .groups = "keep") %>%
  mutate(across(c(x_rc, lower, upper), ~ pmax(., 0))) # set negative values to zero


plot_reldiag(df_reldiag) +
  facet_grid(quantile ~ model) +
  coord_cartesian(xlim = c(0, 17500), ylim = c(0, 17500))

# ggsave("figures/4_national_reliability.pdf", width = 160, height = 160, unit = "mm", device = "pdf", dpi = 300)
