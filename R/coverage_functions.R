library(tidyverse)

coverage <- function(df) {
  df %>%
    group_by(model, quantile) %>%
    summarize(l = mean(truth < value), u = mean(truth <= value), .groups = "drop")
}

# resample groups
# in our case dates - with multiple rows for different locations, models, etc.
sample_n_groups <- function(grouped_df, n, replace = FALSE) {
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
    summarize(
      l_5  = quantile(l, 0.05),
      l_25 = quantile(l, 0.25),
      l_75 = quantile(l, 0.75),
      l_95 = quantile(l, 0.95),
      u_5  = quantile(u, 0.05),
      u_25 = quantile(u, 0.25),
      u_75 = quantile(u, 0.75),
      u_95 = quantile(u, 0.95),
      .groups = "drop"
    )
}

# for probability p, number of trials n, determine [q_low, q_up],
# so that a Bin(n, p) random variable is with probability nom_level in this interval
get_consistency_interval <- function(p, n, nom_level) {
  q_low <- qbinom((1 - nom_level) / 2, n, p, lower.tail = TRUE)
  q_up <- qbinom((1 - nom_level) / 2, n, p, lower.tail = FALSE)
  data.frame(q_low, q_up) %>%
    set_names(paste0(c("lower", "upper"), nom_level * 100))
}

# compute consistency bands for each quantile level
get_consistency_bands <- function(df) {
  df %>%
    group_by(model, quantile) %>%
    summarize(count = n(), .groups = "drop") %>%
    mutate(
      get_consistency_interval(quantile, count, 0.9) / count,
      get_consistency_interval(quantile, count, 0.5) / count
    ) %>%
    select(-count)
}

plot_coverage <- function(df,
                          date_column = target_end_date,
                          B = 100,
                          band_type = "None") {
  date_column <- enquo(date_column)

  # some customizations used in all plots
  my_theme <- list(
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = function(x) ifelse(x == 0, "0", x)),
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)),
    theme_bw(base_size = 11),
    theme(
      panel.grid.major = element_line(size = 0.05),
      panel.grid.minor = element_line(size = 0.05)
    ),
    xlab("Quantile level"),
    ylab("Coverage"),
    coord_fixed()
  )

  if (band_type == "confidence") {
    bands <- get_confidence_bands(df, date_column, B)
    bands_layer <- list(
      geom_ribbon(data = bands, aes(x = quantile, ymin = l_5, ymax = l_95), fill = "darkblue", alpha = 0.2),
      geom_ribbon(data = bands, aes(x = quantile, ymin = u_5, ymax = u_95), fill = "darkred", alpha = 0.2)
    )
  } else if (band_type == "confidence2") {
    bands <- get_confidence_bands(df, date_column, B)
    bands_layer <- list(
      geom_ribbon(data = bands, aes(x = quantile, ymin = l_25, ymax = l_75), fill = "darkred", alpha = 0.3),
      geom_ribbon(data = bands, aes(x = quantile, ymin = l_5, ymax = l_95), fill = "darkred", alpha = 0.2)
    )
  } else if (band_type == "consistency") {
    bands <- get_consistency_bands(df)
    bands_layer <- list(
      geom_ribbon(data = bands, aes(x = quantile, ymin = lower50, ymax = upper50), fill = "skyblue3", alpha = 0.3),
      geom_ribbon(data = bands, aes(x = quantile, ymin = lower90, ymax = upper90), fill = "skyblue3", alpha = 0.2)
    )
  }

  # compute coverage on full sample
  coverage_full <- coverage(df)

  ggplot(coverage_full) +
    facet_wrap("model", ncol = 3) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size = 0.2, linetype = "solid", colour = "grey70") +
    {
      if (band_type != "None") bands_layer
    } +
    geom_errorbar(aes(x = quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
    my_theme
}
