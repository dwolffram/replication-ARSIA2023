library(tidyverse)
library(gridExtra)
Sys.setlocale("LC_ALL", "C")


elementary_quantile_score <- function(y_true, y_pred, theta, alpha){
  ((y_true < y_pred) - alpha) * ((theta < y_pred) - (theta < y_true))
}

get_thetas <- function(value, truth, n=1001){
  tmp <- c(value, truth)
  thetas <- seq(from = min(tmp) - 0.1, to = max(tmp) + 0.1, length.out = n)
  return(thetas)
}

get_elementary_scores <- function(truth, value, quantile, n) {
  thetas <- get_thetas(value, truth, n)

  scores <- sapply(thetas,
                   function(t) mean(elementary_quantile_score(truth, value, t, quantile)))

  return(data.frame(theta=thetas, mean_score=scores))
}

quantile_score <- function(y_true, y_pred, alpha){
  return((1 * (y_true < y_pred) - alpha) * (y_pred - y_true))
}

murphydiag <- function(data, alpha, digits=3) {
  df <- filter(data, quantile == alpha)

  score_label <- df %>%
    mutate(qs = quantile_score(truth, value, quantile)) %>%
    group_by(model, quantile) %>%
    summarize(mean_qs = mean(qs),
              label = paste0(unique(model), " (", round(mean_qs, digits = digits),
                             ")", collapse = "\n"),
              .groups = "drop")

  df <- df %>%
    group_by(model, quantile) %>%
    summarize(get_elementary_scores(truth, value, quantile, n=500), .groups = "drop")

  df <- df %>%
    left_join(score_label, by = c("model", "quantile"))

  return(df)
}
