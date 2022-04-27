library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)

source("R/coverage_functions.R")
source("Routines_Kristof/reliability_functions.R")
source("R/murphy_diagram_functions.R")

QUANTILE <- 0.75
N_RES_RELIABILITY <- 99
DIGITS <- 3

my_pred <- read_csv("data/GEFCom14_Wind_Forecasts.csv.gz")

# Visualize forecasts ------------------------------------------------------------------------
df <- filter(my_pred, target_end_date >= "2013-01-01 12:00:00",
             target_end_date <= "2013-01-08 12:00:00")

zero_hour <- hour(df$target_end_date) == 0
# drop first date as it is not displayed and pick 2nd and 6th date to display on x-axis
my_dates <- unique(df$target_end_date[zero_hour])[c(2, 6)]

median_and_truth <- filter(df, quantile == 0.5)

levels <- c(90, 80, 50, 20)
abs_alpha <- setNames(1 - levels / 100, paste0(levels, "%"))
# since we draw bands over each other we need to know what are the alpha differences
rel_alpha <- abs_alpha - lag(abs_alpha, default=0)
lower_quantiles <-(100 - levels) / 2
upper_quantiles <- 100 - lower_quantiles

df_lower <- filter(df, (100 * quantile) %in% lower_quantiles) %>%
  mutate(level = paste0((1 - quantile * 2) * 100, "%")) %>%
  rename(lower = value) %>%
  select(model, target_end_date, level, lower)

df_upper <- filter(df, (100 * quantile) %in% upper_quantiles) %>%
  mutate(level = paste0((1 - (1 - quantile) * 2) * 100, "%")) %>%
  rename(upper = value) %>%
  select(model, target_end_date, level, upper)

bands <- left_join(df_lower, df_upper, by=c("model", "target_end_date", "level"))

forecast_plot <- ggplot(mapping=aes(x=target_end_date)) +
  facet_grid(""~model) +  # denote row with empty string to align with following plots
  geom_ribbon(data=bands,
              aes(ymin=lower, ymax=upper, group=level, alpha=level), fill='deepskyblue4') +
  geom_line(data=median_and_truth, aes(y=value, group=1, color="Median")) +
  geom_line(data=median_and_truth, aes(y=truth, group=1, color="Truth")) +
  scale_x_datetime(date_minor_breaks = "days", breaks = as.POSIXct(my_dates)) +
  scale_color_manual(name=NULL, values=c("Truth"="darkred", "Median"="deepskyblue3"),
                     guide=guide_legend(order=1, direction="horizontal")) +
  scale_alpha_manual(name="Prediction interval", values=rel_alpha,
                     guide=guide_legend(order=2, override.aes=list(alpha=abs_alpha),
                                        direction="horizontal", title.position="top", title.hjust=1)) +
  xlab(NULL) +
  ylab('Wind power output') +
  expand_limits(y = 1.15) +
  scale_y_continuous(breaks=0:4 / 4, labels=function(x) ifelse(x == 0, "0", x)) +
  theme_bw(base_size = 11)  +
  theme(legend.justification=c(1,1), legend.position=c(0.99,0.99),
        legend.box="horizontal",
        legend.background=element_blank(), legend.box.background=element_blank(),
        legend.box.just="right", legend.title=element_text(size=6, face = "bold"),
        legend.text=element_text(size=6),
        legend.margin=margin(1,2,1,25),   # distance between two legends
        legend.key.size = unit(0.4, "lines"),
        panel.grid.major = element_line(size = 0.05), # linewidth of background grid
        panel.grid.minor = element_line(size = 0.05), # linewidth of background grid
        strip.background.y = element_blank())         # remove grey box from facet_grid in rows


# Construct coverage plots ---------------------------------------------------------------------
coverage_df <- my_pred %>% 
  group_by(model, quantile) %>% 
  coverage(band_type = "none")

coverage_plot <- plot_coverage(coverage_df) + 
  facet_grid(""~model) +
  theme(panel.grid.major = element_line(size = 0.05),
        panel.grid.minor = element_line(size = 0.05),
        strip.background.x = element_blank(), strip.text.x = element_blank(),
        strip.background.y = element_blank())


# Construct reliability diagram ----------------------------------------------------------------
recal_and_bands <- plot_reliability(filter(my_pred, quantile ==  QUANTILE),
                                    N_RES_RELIABILITY, digits = DIGITS)


# Construct murphy diagram ---------------------------------------------------------------------
df <- murphydiag(filter(my_pred, quantile ==  QUANTILE), digits = DIGITS)
murphy_diagram <- ggplot(df) +
  facet_wrap(~quantile, strip.position="right") +
  scale_x_continuous(breaks = 0:4 / 4, labels=function(x) ifelse(x == 0, "0", x))


# Combine everything --------------------------------------------------------------------------
fig10 <- grid.arrange(forecast_plot, coverage_plot, reliability_diagram, murphy_diagram,
             ncol=1, heights=c(0.258, 0.25, 0.25, 0.242))
ggsave("figures/10_GEFCom14_CaseStudy.pdf", plot=fig10, width=160, height=200,
       unit="mm", device="pdf", dpi=300)
