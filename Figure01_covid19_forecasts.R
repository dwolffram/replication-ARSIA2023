library(tidyverse)
#Sys.setlocale("LC_ALL", "C")

MODELS <- c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")
HIGHLIGHT <- c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)

df <- read_csv("data/covid19-preprocessed.csv.gz", col_types = cols()) %>%
  filter(
    location == "US",
    model %in% MODELS
  ) %>%
  rename(qlevel = quantile)

df1 <- df %>% 
  filter(qlevel %in% HIGHLIGHT)

df2 <- df %>%
  filter(!qlevel %in% HIGHLIGHT)

df_box <- df %>%
  filter(qlevel %in% c(0.01, 0.5, 0.99)) %>% 
  pivot_wider(names_from = qlevel, names_prefix = "value.", values_from = value)

ggplot(df1, aes(x=target_end_date)) +
  facet_wrap("model", ncol = 1) +
  geom_crossbar(data = df_box, aes(y = value.0.5, ymin = value.0.01, ymax = value.0.99), fatten = 1,
                width = 2, size = 0.3, colour = "azure4", fill = "gray", alpha = 0.4) +
  geom_segment(data = df2, aes(x = target_end_date - 1, xend = target_end_date + 1, y = value, yend = value), 
               color = "azure4", size = 0.25) + 
  geom_segment(data = df1, aes(x = target_end_date - 1.25, xend = target_end_date + 1.25, y = value, yend = value), 
               color = "deepskyblue4", size = 0.4, lineend = "round") + 
  geom_line(aes(y=truth, col='darkred'), size = 0.8) +
  scale_color_identity(name = NULL,
                       breaks = c("darkred"),
                       labels = c("Truth"),
                       guide = "legend") +
  scale_x_date(date_breaks = "months" , date_labels = "%b", expand = c(0.02, 0)) +
  xlab("2021") +
  ylab('Incident deaths') +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_blank(),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(hjust = -1.25))

# ggsave("figures/1_covid19_forecasts.pdf", width=160, height=200, unit="mm", device = "pdf")
