source("models_Engel.R")
source("R/reliability_functions.R")

set.seed(100)

n_resamples <- 999
digits <- 1
quantile <- 0.1

reldiag_Engel <- data_long %>%
  filter(quantile == !!quantile) %>%
  group_by(model, type) %>%
  summarize(reldiag(value, truth, alpha = unique(quantile), n_resamples = n_resamples, resample_log = TRUE, digits = digits), .groups = "keep") %>%
  mutate(across(c(x_rc, lower, upper), ~ pmax(., 0))) %>%
  mutate_at(c("lower", "upper"), ~ replace(., model == model_id$iso && type == is, x_rc))

# custom score decomposition layer
scores <- reldiag_Engel %>%
  distinct(across(score:pval_ucond)) %>%
  # mutate(quantile = quantile) %>%
  mutate(label = paste0(c("\nuMCB ", "cMCB ", "DSC ", "UNC "),
    c(
      ifelse(type == is, format(round(umcb, digits = digits), nsmall = 0),
        format(round(umcb, digits = digits), nsmall = digits)
      ),
      ifelse(type == is & model == model_id[3], format(round(cmcb, digits = digits), nsmall = 0),
        format(round(cmcb, digits = digits), nsmall = digits)
      ),
      format(round(c(dsc, unc), digits = digits), nsmall = digits)
    ),
    # round(c(umcb, cmcb, dsc, unc), digits = digits),
    c(ifelse(type == is, "", paste0(" [p = ", round(pval_ucond, digits = 2), "]")), "", "", ""),
    c("", ifelse(type == is & model == model_id[3], "", paste0(" [p = ", round(pval_cond, digits = 2), "]")), "", ""),
    collapse = "\n"
  ))
score_layer <- list(
  geom_label(
    data = scores, mapping = aes(x = 0, y = Inf, label = sprintf(paste0("bar(S)~'%0.", digits, "f'"), score)),
    size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA, alpha = 0, label.padding = unit(1, "lines"), parse = TRUE
  ),
  geom_label(
    data = scores, mapping = aes(x = 0, y = Inf, label = label),
    size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA, alpha = 0, label.padding = unit(1, "lines"), parse = FALSE
  )
)

plot_reldiag(reldiag_Engel, score_decomp = FALSE) +
  facet_grid(rows = vars(type), cols = vars(model)) +
  score_layer +
  scale_x_log10(guide = guide_axis(check.overlap = TRUE)) +
  scale_y_log10()

# ggsave(filename = "figures/9_Engel_reliability.pdf", width = 160, height = 110, device = "pdf", units = "mm")

# Table 1
scores <- data_long %>%
  group_by(model, type, quantile) %>%
  summarize(reldiag(value, truth, alpha = unique(quantile), resampling = FALSE, digits = digits)) %>%
  distinct(across(c(model:quantile, score:unc))) %>%
  select(!mcb) %>%
  transmute(across(score:unc, round, digits))
table <- melt(scores, id.vars = c("model", "quantile", "type")) %>% dcast(formula = quantile + variable ~ model + type, mean)

table[c(
  "quantile", "variable", "Linear_In-sample", "Log-linear_In-sample", "Isotonic_In-sample",
  "Linear_Out-of-sample", "Log-linear_Out-of-sample", "Isotonic_Out-of-sample"
)]
