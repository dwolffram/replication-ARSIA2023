library(tidyverse)
library(isotone)

reldiag <- function(x, y, alpha = 0.5, n_resamples = 999, digits = 1, region_level = 0.9) {
  pava <- function(x, y) {
    # In case of ties, isotone::gpava uses the conditional mean instead of quantile, try e.g.,
    # gpava(c(-1,-1,-1),c(-1,0,0),solver = weighted.median,ties = "secondary")

    # Wrong fix: The following step replaces y values with the respective quantile in case of ties
    # y = unlist(lapply(split(y,x),function(y) rep(quantile(y,alpha,type = 1),length(y))),use.names = FALSE)

    # New fix: Use ranking of predictor values and break ties by ordering the corresponding instances in order of decreasing observations
    ranking <- match(1:length(x), order(x, y, decreasing = c(FALSE, TRUE)))

    return(gpava(ranking, y, solver = weighted.fractile, p = alpha, ties = "secondary")$x)
  }
  score <- function(x, y) mean((as.numeric(x >= y) - alpha) * (x - y))
  marg <- function(x) quantile(x, alpha, type = 1)
  identif <- function(x, y) as.numeric(x > y) - alpha
  score_label <- "QS "

  ord_x <- order(x)
  x <- x[ord_x]
  y <- y[ord_x]

  x_rc <- pava(x, y)

  res <- y - x

  s <- score(x, y)
  c_rc_ucond <- optim(par = 0, fn = function(c) score(x + c, y), method = "Brent", lower = min(res), upper = max(res))$par
  s_rc_ucond <- score(x + c_rc_ucond, y)
  s_rc <- score(x_rc, y)
  s_mg <- score(marg(y), y)

  mcb <- s - s_rc
  umcb <- s - s_rc_ucond
  cmcb <- s_rc_ucond - s_rc
  dsc <- s_mg - s_rc
  unc <- s_mg

  # The Score is exactly equal to uMCB + cMCB - DSC + UNC.
  # However, when rounding the values there may be slight discrepancies between the rounded values.
  # We avoid this for didactic reasons by computing the score from the rounded values.
  s <- sum(round(c(umcb, cmcb, -dsc, unc), digits))


  # test: mean identification zero? (t-test)
  # v = identif(x,y)
  # t = sqrt(length(v)) * mean(v)/sd(v)
  # pval_ucond = 1 - abs(pt(t,length(v)-1) - 0.5)*2

  # Unconditional calibration test
  # Coverage test: One-sided Binomial tests with Bonferroni correction
  eps <- 10^-10 # avoid numerical artifacts by assuming that values with an absolute difference of less than eps are identical
  hard_cov <- sum(y < x - eps)
  soft_cov <- sum(y < x + eps)

  pval_hard <- dbinom(hard_cov, length(y), alpha) + pbinom(hard_cov, length(y), alpha, FALSE)
  pval_soft <- pbinom(soft_cov, size = length(y), prob = alpha)
  pval_ucond <- min(pval_hard, pval_soft, 0.5) * 2
  # print(paste0("p-Values: hard ",pval_hard,", soft ",pval_soft))

  n_samples <- n_resamples + 1 # total number of samples including observed sample
  low <- floor(n_samples * (1 - region_level) / 2)
  up <- n_samples - low
  pval_digits <- ceiling(log(n_samples, 10))

  resamples <- sapply(1:n_resamples, function(i) x + sample(res, length(y), replace = TRUE))

  x_rc_resamples <- apply(resamples, 2, function(y) pava(x, y))
  x_rc_resamples_sorted <- apply(cbind(x_rc, x_rc_resamples), 1, sort) - marg(res) # includes observed values + bias corrected (shifted by mean residual)

  ran_x <- range(x)

  mcb_resamples <- sapply(1:n_resamples, function(i) score(x, resamples[, i]) - score(x_rc_resamples[, i], resamples[, i]))
  mcb_bounds <- sort(c(mcb, mcb_resamples))[c(low, up)]

  rank_obs <- tail(rank(c(mcb_resamples, mcb)), 1)
  pval <- 1 - (rank_obs - 1) / (n_resamples + 1)

  results <- data.frame(
    quantile = alpha, x = x, y = y, x_rc = x_rc,
    lower = x_rc_resamples_sorted[low, ],
    upper = x_rc_resamples_sorted[up, ],
    score = s,
    umcb = umcb, cmcb = cmcb, mcb = mcb, dsc = dsc, unc = unc,
    pval_cond = pval, pval_ucond = pval_ucond
  )
}

plot_reliability <- function(df, n_resamples = 99, digits = 1) {
  # compute recalibration, consistency band and score decomposition
  df_reldiag <- df %>%
    group_by(model, quantile) %>%
    summarize(reldiag(value, truth, alpha = unique(quantile), n_resamples = n_resamples), .groups = "drop") %>%
    mutate(across(c(x_rc, lower, upper), ~ pmax(., 0))) # set negative values to zero

  # summarize scores and create labels
  scores <- df_reldiag %>%
    group_by(model, quantile) %>%
    distinct(across(score:pval_ucond)) %>%
    mutate(label = paste0(c("\nuMCB ", "cMCB ", "DSC ", "UNC "),
                          format(round(c(umcb, cmcb, dsc, unc), digits = digits),
                                 nsmall = digits, trim = TRUE),
                          collapse = " \n")
    )

  # needed to ensure square facets with equal x and y limits
  facet_lims <- df_reldiag %>%
    group_by(model, quantile) %>%
    summarize(
      mn = min(c_across(c(x, x_rc, lower, upper))),
      mx = max(c_across(c(x, x_rc, lower, upper)))
    )

  ggplot(df_reldiag, aes(x, x_rc, group = model)) +
    facet_grid(rows = vars(quantile), cols = vars(model)) +
    geom_point(aes(x, y), , alpha=0.05, size=0.05) +
    geom_abline(intercept = 0, slope = 1, colour = "grey70") +
    geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
    geom_line(aes(x,lower), color = "deepskyblue2") +
    geom_line(aes(x,upper), color = "deepskyblue2") +
    geom_line(color = "firebrick3") +
    geom_blank(data = facet_lims, aes(x = mx, y = mx)) +
    geom_blank(data = facet_lims, aes(x = mn, y = mn)) +
    xlab("Forecast value") +
    ylab("Conditional quantile") +
    geom_label(
      data = scores, mapping = aes(x = -Inf, y = Inf, label = sprintf("bar(S)~'%0.1f'", score)),
      size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA, alpha = 0, label.padding = unit(1, "lines"), parse = TRUE
    ) +
    geom_label(
      data = scores, mapping = aes(x = -Inf, y = Inf, label = label),
      size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA, alpha = 0, label.padding = unit(1, "lines"), parse = FALSE
    ) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks=0:4 / 4,
                     labels=function(x) ifelse(x == 0, "0", x)) +
    scale_y_continuous(breaks=0:4 / 4, labels=function(x) ifelse(x == 0, "0", x)) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(size = 0.05),
      panel.grid.minor = element_line(size = 0.05),
      strip.text.x = element_text(size = 7),
      strip.text.y = element_text(size = 7),
      strip.background.x = element_blank(),  # no facet boxes in x direction
      strip.text.x = element_blank()         # no facet texts in x direction)
    )
}
