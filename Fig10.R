library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)

source("Routines_Kristof/coverageplot_function.R")
source("Routines_Kristof/reldiag_function.R")
source("Routines_Kristof/murphydiag_function.R")

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
df <- coverageplot(my_pred, type = "None")

coverage_plot <- ggplot(df) +
  facet_grid(""~model) +  # want to denote row with empty string (to have alignment)
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size = 0.2, linetype = "solid",
               colour = "grey70") +
  geom_errorbar(aes(x=quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  xlab("Quantile level") +
  ylab("Coverage") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(size = 0.05),
        panel.grid.minor = element_line(size = 0.05),
        strip.background.x = element_blank(), strip.text.x = element_blank(),
        strip.background.y = element_blank())


# Construct reliability diagram ----------------------------------------------------------------
recal_and_bands <- filter(my_pred, quantile ==  QUANTILE) %>%
  group_by(model) %>%
  summarise(reldiag(value, truth, alpha=QUANTILE, n_resamples=N_RES_RELIABILITY, digits=DIGITS),
            .groups="drop") %>%
  # target var is normalized
  mutate(lower = pmin(1, pmax(0, lower)), upper = pmin(1, pmax(0, upper))) %>%
  mutate_at(c("x_rc", "lower", "upper"), ~ replace(., .<0, 0))

all_scores <- recal_and_bands %>%
  group_by(model) %>%
  distinct(across(score:pval_ucond))

scores <- mutate(alls_scores, label = paste0(c("\nuMCB ","cMCB ","DSC ","UNC "),
                        format(c(umcb, cmcb, dsc, unc), nsmall=DIGITS),
                        collapse = "\n"))

qs_scores <- select(all_scores, score)

reliability_diagram <- ggplot(recal_and_bands, aes(x, x_rc, group=model)) +
  facet_grid(quantile ~ model) +
  geom_point(aes(x, y), alpha=0.05, size=0.05) +
  geom_abline(intercept = 0 , slope = 1, colour="grey70") +
  geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
  geom_line(aes(x,lower), color = "deepskyblue2") +
  geom_line(aes(x,upper), color = "deepskyblue2") +
  geom_line(color = "firebrick3") +
  xlab("Forecast value") +
  ylab("Conditional quantile") +
  geom_label(data = scores, mapping = aes(x = -Inf, y = Inf, label = label),
             size = 6*0.36, hjust = 0, vjust = 1, label.size = NA, alpha=0,
             label.padding = unit(1, "lines")) +
  geom_label(data = qs_scores, mapping = aes(x = -Inf, y = Inf,
                                             label = paste0("bar(S)~", score)),parse = TRUE,
             size = 6*0.36, hjust = 0, vjust = 1, label.size = NA,
             alpha=0, label.padding = unit(1, "lines")) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks=0:4 / 4,
                     labels=function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(breaks=0:4 / 4, labels=function(x) ifelse(x == 0, "0", x)) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(size = 0.05),
        panel.grid.minor = element_line(size = 0.05),
        legend.justification=c(1,0), legend.position=c(0.99,0.01),
        legend.background=element_blank(), legend.box.background=element_blank(),
        legend.title=element_text(size=6, face = "bold"),
        legend.text=element_text(size=6), legend.key.size = unit(0.4, "lines"),
        strip.background.x = element_blank(),  # no facet boxes in x direction
        strip.text.x = element_blank())        # no facet texts in x direction)


# Construct murphy diagram ---------------------------------------------------------------------
df <- murphydiag(my_pred, QUANTILE, digits = DIGITS)
ymax = max(df$mean_score)
xmax = max(df$theta)

murphy_diagram <- ggplot(df) +
  facet_wrap(~quantile, strip.position="right") +
  geom_line(aes(x=theta, y=mean_score, color=label), size=0.5) +
  xlab(expression(paste("Threshold ", theta))) +
  ylab("Elementary score") +
  theme_bw(base_size = 11) +
  theme(legend.justification=c(1,1), legend.position=c(0.99,0.99),
        legend.title=element_text(size=6, face = "bold"), legend.text=element_text(size=6),
        legend.title.align = 0, legend.text.align = 0,
        legend.key.size = unit(0.2, "lines"), legend.background = element_blank(),
        panel.grid.major = element_line(size = 0.05),
        panel.grid.minor = element_line(size = 0.05)) +
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(breaks = 0:4 / 4, labels=function(x) ifelse(x == 0, "0", x)) +
  labs(color = "Model (pinball loss)") +
  expand_limits(x = xmax, y = ymax)


# Combine everything --------------------------------------------------------------------------
fig10 <- grid.arrange(forecast_plot, coverage_plot, reliability_diagram, murphy_diagram,
             ncol=1, heights=c(0.258, 0.25, 0.25, 0.242))
ggsave("Figure10.pdf", plot=fig10, width=160, height=200, unit="mm", device="pdf", dpi=300)