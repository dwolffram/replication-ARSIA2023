source("models_Engel.R")
source("R/reliability_functions.R")

set.seed(100)

n_resamples <- 999
digits <- 1
quantile <- 0.1

reldiag_Engel <- data_long %>%
  filter(quantile == !!quantile) %>%
  group_by(model, type) %>%
  summarize(reldiag(value, truth, alpha = unique(quantile), n_resamples = n_resamples,
                    resample_log = TRUE, digits = digits), .groups = "keep") %>%
  mutate(across(c(x_rc, lower, upper), ~ pmax(., 0))) %>%
  mutate_at(c("lower", "upper"), ~ replace(., model == model_id$iso && type == ins, x_rc))

# custom score decomposition layer
digits_f <- paste0("%.", digits, "f")
scores <- reldiag_Engel %>%
  distinct(across(score:pval_ucond)) %>%
  mutate(label = paste0(
    "\n",
    ifelse(type == ins,
           sprintf("uMCB %.0f\n", ifelse(abs(umcb) < 1e-6, 0.0, umcb)),
           sprintf("uMCB %s [p = %.2f]\n", sprintf(digits_f, umcb), pval_ucond)),
    ifelse(type == ins & model == model_id[3],
           sprintf("cMCB %.0f\n", ifelse(abs(cmcb) < 1e-6, 0.0, cmcb)),
           sprintf("cMCB %s [p = %.2f]\n", sprintf(digits_f, cmcb), pval_cond)),
    sprintf("DSC %s\n", sprintf(digits_f, dsc)),
    sprintf("UNC %s", sprintf(digits_f, unc))
  ))
score_layer <- list(
  geom_label(
    aes(x = 0, y = Inf, label = sprintf("bar(S)~'%s'", sprintf(digits_f, score))),
    data = scores, size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA,
    alpha = 0, label.padding = unit(1, "lines"), parse = TRUE
  ),
  geom_label(
    aes(x = 0, y = Inf, label = label),
    data = scores, size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA,
    alpha = 0, label.padding = unit(1, "lines"), parse = FALSE
  )
)

plot_reldiag(reldiag_Engel, score_decomp = FALSE) +
  facet_grid(rows = vars(type), cols = vars(model)) +
  score_layer +
  scale_x_log10(guide = guide_axis(check.overlap = TRUE)) +
  scale_y_log10()

# ggsave(filename = "figures/09_Engel_reliability.pdf", width = 160, height = 110, device = "pdf", units = "mm")

# Table 1
scores <- data_long %>%
  group_by(model, type, quantile) %>%
  summarize(reldiag(value, truth, alpha = unique(quantile), resampling = FALSE, digits = digits)) %>%
  distinct(across(c(model:quantile, score:unc))) %>%
  select(!mcb) %>%
  transmute(across(score:unc, round, digits))
table <- pivot_longer(data = scores, cols = score:unc, names_to = "component") %>%
  pivot_wider(names_from = c(model, type)) %>%
  select(c("quantile", "component", "Linear_In-sample", "Log-linear_In-sample",
           "Isotonic_In-sample", "Linear_Out-of-sample",
           "Log-linear_Out-of-sample", "Isotonic_Out-of-sample"))

print(as.data.frame(table))
