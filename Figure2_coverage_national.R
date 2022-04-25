library(tidyverse)
library(patchwork)
source("R/functions_Engel.R")

# Load data
MODELS <- c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location == "US",
    target == "1 wk ahead inc death",
    model %in% MODELS
  ) %>%
  mutate(value = floor(value))

coverage1 = df %>%
  group_by(model, quantile) %>%
  coverage(type = "consistency")
p1 = plot.coverage(coverage1) +
  facet_wrap("model") +
  facet_grid("Consistency" ~ model) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 8),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    axis.title.y = element_text(hjust = -0.35)
  )

coverage2 = df %>%
  group_by(model, quantile) %>%
  coverage(type = "confidence2", B = 100)
p2 = plot.coverage(coverage2) +
  facet_wrap("model") +
  facet_grid("Confidence" ~ model) +
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    plot.background = element_blank()
  ) +
  labs(y = element_blank())

p1 / p2

# ggsave("figures/2_national_coverage.pdf", width = 160, height = 110, unit = "mm", device = "pdf", dpi = 300)
