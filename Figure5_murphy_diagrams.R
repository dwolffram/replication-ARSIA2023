library(tidyverse)
library(patchwork)
source("R/murphy_diagram_functions.R")

MODELS <- c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")
QUANTILES <- c(0.25, 0.5, 0.75)

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location != "US",
    target == "1 wk ahead inc death",
    model %in% MODELS,
    quantile %in% QUANTILES
  )

df$model <- str_replace(df$model, "KITmetricslab-select_ensemble", "KITmetricslab")


df_murphy <- murphydiag(df)

p1 <- df_murphy %>%
  filter(quantile == 0.25) %>%
  plot_murphy_diagram() +
  xlab(NULL)

p2 <- df_murphy %>%
  filter(quantile == 0.5) %>%
  plot_murphy_diagram() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab(NULL)

p3 <- df_murphy %>%
  filter(quantile == 0.75) %>%
  plot_murphy_diagram() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + xlab(NULL) + ylab(NULL)

g <- p1 + p2 + p3

g

ggsave("figures/5_states_murphy.pdf", plot = g, width = 160, height = 70, unit = "mm", device = "pdf", dpi = 300)
