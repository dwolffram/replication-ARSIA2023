library(tidyverse)
Sys.setlocale("LC_ALL", "C")

coverage <- function(df) {
  df %>%
    group_by(model, quantile) %>%
    summarize(l = mean(truth < value), u = mean(truth <= value), .groups = "drop")
}

# resample groups
# in our case dates - with multiple rows for different locations, models, etc.
sample_n_groups = function(grouped_df, n, replace = FALSE) {
  groups_resampled <- grouped_df %>%
    group_keys() %>%
    slice_sample(n = n, replace = replace)

  grouped_df %>%
    right_join(groups_resampled, by = group_vars(grouped_df))
}

get_confidence_bands <- function(df, date_column, B = 100) {
  coverage_df <- data.frame()

  for (i in 1:B) {
    df_resampled <- df %>%
      group_by(!!date_column) %>%
      sample_n_groups(n = n_distinct(group_keys(.)), replace = TRUE)

    coverage_sample <- coverage(df_resampled)

    coverage_df <- bind_rows(coverage_df, coverage_sample)
  }

  # compute CIs from bootstrapped coverage
  coverage_df %>%
    group_by(model, quantile) %>%
    summarize(l_5 = quantile(l, 0.05),
              l_95 = quantile(l, 0.95),
              l_25 = quantile(l, 0.25),
              l_75 = quantile(l, 0.75),
              u_5 = quantile(u, 0.05),
              u_95 = quantile(u, 0.95),
              u_25 = quantile(u, 0.25),
              u_75 = quantile(u, 0.75),
              .groups = "drop")
}

# for probability p, number of trials n, determine [q_low, q_up],
# so that a Bin(n, p) random variable is with probability nom_level in this interval
get_consistency_interval <- function(p, n, nom_level) {
  q_low <- qbinom((1 - nom_level) / 2, n, p, lower.tail = TRUE)
  q_up  <- qbinom((1 - nom_level) / 2, n, p, lower.tail = FALSE)
  data.frame(q_low, q_up) %>%
    set_names(paste0(c("lower", "upper"), nom_level * 100))
}

# compute consistency bands for each quantile level
get_consistency_bands <- function(df) {
  df %>%
    group_by(model, quantile) %>%
    summarize(count = n(), .groups = "drop") %>%
    mutate(get_consistency_interval(quantile, count, 0.9) / count,
           get_consistency_interval(quantile, count, 0.5) / count) %>%
    select(-count)
}

coverageplot <- function(df,
                         date_column = target_end_date,
                         B = 100,
                         type = "confidence",
                         difference = FALSE) {

  # compute coverage on full sample
  coverage_full <- coverage(df)
  results <- coverage_full
  my_vars <- c("l", "u")

  if (type %in% c("confidence", "confidence2")) {
    my_vars <- c("l", "u", "l_5", "l_25", "l_75", "l_95", "u_5", "u_25","u_75", "u_95")
    date_column <- enquo(date_column)
    bands <- get_confidence_bands(df, date_column, B)

    results <- coverage_full %>%
      left_join(bands, by = c("model", "quantile"))

  } else if (type == "consistency") {
    my_vars <- c("l", "u", "lower50", "upper50", "lower90", "upper90")
    bands <- get_consistency_bands(df)

    results <- coverage_full %>% left_join(bands, by = c("model", "quantile"))
  }

  if (difference) {
    results <- results %>%
        mutate_at(vars(my_vars), list(~ . - quantile))
  }
  return(results)
}