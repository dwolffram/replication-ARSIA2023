library(tidyverse)
library(patchwork)
source("R/murphy_diagram_functions.R")

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location != "US",
    model %in% c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline"),
    quantile %in% c(0.25, 0.5, 0.75)
  )

df$model <- str_replace(df$model, "KITmetricslab-select_ensemble", "KITmetricslab")

df_murphy <- murphydiag(df)

ymax <- max(df_murphy$mean_score)
xmax <- max(df_murphy$theta)

p1 <- df_murphy %>%
  filter(quantile == 0.25) %>%
  plot_murphy_diagram() +
  xlab(NULL) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y))

p2 <- df_murphy %>%
  filter(quantile == 0.5) %>%
  plot_murphy_diagram() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab(NULL)

p3 <- df_murphy %>%
  filter(quantile == 0.75) %>%
  plot_murphy_diagram() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + xlab(NULL) + ylab(NULL)

g <- p1 + p2 + p3

g <- g + expand_limits(x = xmax, y = ymax)

# ggsave("figures/5_states_murphy.pdf", plot = g, width = 160, height = 70, unit = "mm", device = "pdf", dpi = 300)
