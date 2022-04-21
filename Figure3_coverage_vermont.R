library(tidyverse)
library(patchwork)
source("R/plot_coverage.R")

# Load data
models <- c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location_name == "Vermont",
    target == "1 wk ahead inc death",
    model %in% models
  ) %>%
  mutate(value = floor(value))

p1 <- plot_coverage(df, B = 100, type = "consistency", difference = FALSE) +
  facet_grid("Consistency" ~ model) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 8),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    axis.title.y = element_text(hjust = -0.35)
  )

p2 <- plot_coverage(df, B = 100, type = "confidence", difference = FALSE) +
  facet_grid("Confidence" ~ model) +
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    plot.background = element_blank()
  ) +
  labs(y = element_blank())

p1 / p2

ggsave("figures/3_Vermont_coverage.pdf", width = 160, height = 110, unit = "mm", device = "pdf", dpi = 300)
