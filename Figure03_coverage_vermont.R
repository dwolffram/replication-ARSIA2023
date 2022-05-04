library(tidyverse)
library(patchwork)
source("R/coverage_functions.R")

set.seed(100)

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location_name == "Vermont",
    model %in% c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")
  ) 

# Upper plot with consistency bands
coverage1 <- df %>%
  group_by(model, quantile) %>%
  coverage(band_type = "consistency")

p1 <- plot_coverage(coverage1) +
  facet_grid("Consistency" ~ model) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 8),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    axis.title.y = element_text(hjust = -0.35)
  )

# Lower plot with confidence bands for hard and soft coverage
coverage2 <- df %>%
  group_by(model, quantile) %>%
  coverage(band_type = "confidence", B = 100)

p2 <- plot_coverage(coverage2) +
  facet_grid("Confidence" ~ model) +
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    plot.background = element_blank()
  ) +
  labs(y = element_blank())

p1 / p2

# ggsave("figures/3_Vermont_coverage.pdf", width = 160, height = 110, unit = "mm", device = "pdf", dpi = 300)
