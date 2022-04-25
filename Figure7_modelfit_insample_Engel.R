source("models_Engel.R")

# Plot in-sample fits
ggplot(data_is_long) +
  facet_grid(cols = vars(model)) +
  geom_point(aes(x=predictor, y=truth),data = subset(data_is_long,quantile == 0.5),alpha=0.2,size = 0.6) +
  geom_line(aes(x=predictor, y=value, group = factor(quantile), color=factor(quantile,levels = sort(q_levels,decreasing = TRUE)))) +
  labs(color = "Level") + xlab("Household income") + ylab("Food expenditure") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_line(size = 0.05)) +
  theme(aspect.ratio = 1)
# ggsave(filename = "figures/7_Engel_modelfit_insample.pdf",width = 160,height = 60,device = "pdf",units = "mm")
