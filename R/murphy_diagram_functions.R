library(tidyverse)

elementary_quantile_score <- function(y_true, y_pred, theta, alpha) {
  ((y_true < y_pred) - alpha) * ((theta < y_pred) - (theta < y_true))
}

get_thetas <- function(value, truth, n = 1001) {
  tmp <- c(value, truth)
  max_val <- max(tmp)
  min_val <- min(tmp)
  thetas <- seq(
    from = min_val - 0.01 * (max_val - min_val),
    to = max_val + 0.01 * (max_val - min_val),
    length.out = n
  )
  return(thetas)
}

get_elementary_scores <- function(truth, value, quantile, n) {
  thetas <- get_thetas(value, truth, n)

  scores <- sapply(
    thetas,
    function(t) mean(elementary_quantile_score(truth, value, t, quantile))
  )

  return(data.frame(theta = thetas, mean_score = scores))
}

quantile_score <- function(y_true, y_pred, alpha) {
  return((1 * (y_true < y_pred) - alpha) * (y_pred - y_true))
}

murphydiag <- function(data, digits = 1) {
  score_label <- data %>%
    mutate(qs = quantile_score(truth, value, quantile)) %>%
    group_by(model, quantile) %>%
    summarize(
      mean_qs = mean(qs),
      label = paste0(unique(model), " (", round(mean_qs, digits = digits),
        ")",
        collapse = "\n"
      ),
      .groups = "drop"
    )

  df <- data %>%
    group_by(model, quantile) %>%
    summarize(get_elementary_scores(truth, value, quantile, n = 500), .groups = "drop")

  df <- df %>%
    left_join(score_label, by = c("model", "quantile"))

  return(df)
}

plot_murphy_diagram <- function(df,
                                aspect_ratio_1 = TRUE) {
  murphy_diag <- ggplot(df) +
    geom_line(aes(x = theta, y = mean_score, color = label), size = 0.5) +
    facet_wrap("quantile", scales = "free") +
    xlab(expression(paste("Threshold ", theta))) +
    ylab("Elementary score") +
    theme_bw(base_size = 11) +
    theme(
      legend.justification = c(1, 1), legend.position = c(0.95, 1),
      legend.title = element_text(size = 6, face = "bold"),
      legend.text = element_text(size = 6),
      legend.title.align = 0,
      legend.text.align = 0,
      legend.key.size = unit(0.2, "lines"),
      legend.background = element_blank(),
      panel.grid.major = element_line(size = 0.05),
      panel.grid.minor = element_line(size = 0.05)
    ) +
    scale_color_brewer(palette = "Set1") +
    labs(color = "Model (pinball loss)")

  if (aspect_ratio_1) {
    murphy_diag <- murphy_diag +
      theme(aspect.ratio = 1)
  }

  return(murphy_diag)
}
