source("models_Engel.R")
source("R/coverage_functions.R")

coverage_Engel <- data_long %>%
  group_by(model, type, quantile) %>%
  coverage(band_type = "consistency") %>%
  mutate_at(
    vars("lower50", "upper50", "lower90", "upper90"),
    list(~ ifelse(type == is, quantile, .))
  )

coverage_plot <- plot_coverage(coverage_Engel) +
  facet_grid(rows = vars(type), cols = vars(model))

coverage_plot

# ggsave(filename = "figures/8_Engel_coverage.pdf",width = 160,height = 110,device = "pdf",units = "mm")
