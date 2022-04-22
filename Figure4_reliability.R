library(tidyverse)
source("R/reliability_functions.R")

set.seed(100)

MODELS <- c("COVIDhub-baseline", "COVIDhub-ensemble", "KITmetricslab-select_ensemble")
QUANTILES <- c(0.25, 0.5, 0.75)
N_RESAMPLES <- 999

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location == "US",
    target == "1 wk ahead inc death",
    model %in% MODELS,
    quantile %in% !!QUANTILES
  )

plot_reliability(df, n_resamples = N_RESAMPLES) +
  coord_cartesian(xlim = c(0, 17500), ylim = c(0, 17500))

ggsave("figures/4_national_reliability.pdf", width = 160, height = 160, unit = "mm", device = "pdf", dpi = 300)