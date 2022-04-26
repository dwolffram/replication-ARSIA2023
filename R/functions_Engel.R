# Coverage
# Compute coverage (returns object of type coverage)
coverage = function(df, 
                    band_type = "confidence",
                     date_column = target_end_date, 
                     B = 100){
  # Functions
  # resample groups
  # in our case dates - with multiple rows for different locations, models, etc.
  sample_n_groups = function(grouped_df, n, replace = FALSE) {
    groups_resampled <- grouped_df %>% 
      group_keys() %>% 
      slice_sample(n = n, replace = replace)
    
    grouped_df %>% 
      right_join(groups_resampled, by = group_vars(grouped_df))
  }
  
  get_confidence_bands <- function(df, date_column, B = 100,df_groups) {
    coverage_df <- data.frame()
    
    for (i in 1:B) {
      df_resampled <- df %>%
        group_by(!!date_column) %>%
        sample_n_groups(n = n_distinct(group_keys(.)), replace = TRUE)
      coverage_sample <- df_resampled %>% 
        group_by(across(df_groups)) %>%
        coverage(band_type = "none")
      
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
      # group_by(model, type, quantile) %>%
      summarize(count = n(), .groups = "drop") %>%
      mutate(get_consistency_interval(quantile, count, 0.9) / count,
             get_consistency_interval(quantile, count, 0.5) / count) %>%
      select(-count)
  }
  
  # grouping variables
  df_groups = group_vars(df)
  
  # avoid numerical artifacts by assuming that values with an absolute difference of less than eps are identical
  eps = 10^-10

  results = df %>% summarize(l = mean(truth < value - eps), u = mean(truth <= value + eps), .groups = "drop")
  
  if (band_type %in% c("confidence", "confidence2")) {
    date_column <- enquo(date_column)
    bands <- get_confidence_bands(df, date_column, B,df_groups = df_groups)
  }
  else if(band_type == "consistency"){
    bands <- get_consistency_bands(df)
  }
  results <- results %>%
    {if(band_type != "none") left_join(.,bands, by = group_vars(df)) else .} %>%
    add_column(band_type = band_type)
     
  return(results)
}

# Plot coverage plot (plots object returned by coverage)
plot.coverage = function(results,
                         difference = FALSE){
  band_type = results$band_type[1]
  
  # some customization used in all plots
  my_theme <- list(
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = function(x) ifelse(x == 0, "0", x)),
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)),
    xlab("Quantile level"),
    ylab("Coverage"),
    theme_bw(base_size = 11),
    theme(panel.grid.major = element_line(size = 0.05), 
          panel.grid.minor = element_line(size = 0.05))
  )
  
  if (band_type == "confidence") {
    bands_layer <- list(
      geom_ribbon(data = results, aes(x = quantile, ymin = l_5, ymax = l_95), fill = "darkblue", alpha = 0.2),
      geom_ribbon(data = results, aes(x = quantile, ymin = u_5, ymax = u_95), fill = "darkred", alpha = 0.2)
    )
  } else if (band_type == "confidence2") {
    bands_layer <- list(
      geom_ribbon(data = results, aes(x = quantile, ymin = l_25, ymax = l_75), fill = "darkred", alpha = 0.3),
      geom_ribbon(data = results, aes(x = quantile, ymin = l_5, ymax = l_95), fill = "darkred", alpha = 0.2)
    )
  } else if (band_type == "consistency") {
    bands_layer <- list(
      geom_ribbon(data = results, aes(x = quantile, ymin = lower50, ymax = upper50), fill = "skyblue3", alpha = 0.3),
      geom_ribbon(data = results, aes(x = quantile, ymin = lower90, ymax = upper90), fill = "skyblue3", alpha = 0.2)
    )
  }
  
  ggplot(results) +
    # facet_wrap("model", ncol = 3) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size = 0.2, linetype = "solid", colour = "grey70") +
    {
      if (!is.na(band_type)) bands_layer
    } +
    geom_errorbar(aes(x = quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
    my_theme
}

# Reliability
# Compute reliability diagram
reldiag = function(x, y, alpha = 0.5, resampling = TRUE, n_resamples = 99, region_level = 0.9,
                   resample_log = FALSE, digits = 2){
  
  require(isotone)
  pava = function(x,y){
    # In case of ties, isotone::gpava uses the conditional mean instead of quantile, try e.g.,
    # gpava(c(-1,-1,-1),c(-1,0,0),solver = weighted.median,ties = "secondary")
    
    # New fix: Use ranking of predictor values and break ties by ordering the corresponding instances in order of decreasing observations
    ranking = match(1:length(x),order(x,y,decreasing = c(FALSE,TRUE)))
    return(gpava(ranking,y,solver = weighted.fractile,p = alpha,ties = "secondary")$x)
  }
  score = function(x, y) mean((as.numeric(x >= y) - alpha)*(x-y))
  marg = function(x) quantile(x, alpha, type = 1)
  identif = function(x, y) as.numeric(x > y) - alpha
  score_label = "QS "
  
  ord_x = order(x)
  x = x[ord_x]
  y = y[ord_x]
  
  x_rc = pava(x,y)
  
  res = y - x
  
  s = score(x,y)
  c_rc_ucond = optim(par = 0,fn = function(c) score(x+c,y),method = "Brent",lower = min(res),upper = max(res))$par
  s_rc_ucond = score(x + c_rc_ucond,y)
  s_rc = score(x_rc,y)
  s_mg = score(marg(y),y)
  
  mcb = s - s_rc
  umcb = s - s_rc_ucond
  cmcb = s_rc_ucond - s_rc
  dsc = s_mg - s_rc
  unc = s_mg
  
  # The Score is exactly equal to uMCB + cMCB - DSC + UNC.
  # However, when rounding the values there may be slight discrepancies between the rounded values.
  # We avoid this for didactic reasons by computing the score from the rounded values.
  s = sum(round(c(umcb,cmcb,-dsc,unc),digits))
  
  
  # test: mean identification zero? (t-test)
  # v = identif(x,y)
  # t = sqrt(length(v)) * mean(v)/sd(v)
  # pval_ucond = 1 - abs(pt(t,length(v)-1) - 0.5)*2
  
  # Unconditional calibration test
  # Coverage test: One-sided Binomial tests with Bonferroni correction
  eps = 10^-10 # avoid numerical artifacts by assuming that values with an absolute difference of less than eps are identical
  hard_cov <- sum(y < x - eps)
  soft_cov <- sum(y < x + eps)
  
  pval_hard = dbinom(hard_cov,length(y),alpha) + pbinom(hard_cov,length(y),alpha,FALSE)
  pval_soft = pbinom(soft_cov,size = length(y),prob = alpha)
  pval_ucond = min(pval_hard,pval_soft,0.5)*2
  # print(paste0("p-Values: hard ",pval_hard,", soft ",pval_soft))
  
  if(resampling){
    n_samples = n_resamples + 1 # total number of samples including observed sample
    low = floor(n_samples * (1-region_level)/2)
    up = n_samples - low
    pval_digits = ceiling(log(n_samples,10))
    
    if(resample_log){
      res_log = log(y) - log(x)
      resamples = sapply(1:n_resamples,function(i) exp(log(x) + sample(res_log,length(y),replace = TRUE))) 
    }
    else resamples = sapply(1:n_resamples,function(i) x + sample(res,length(y),replace = TRUE)) 
    
    x_rc_resamples = apply(resamples, 2, function(y) pava(x,y))
    x_rc_resamples_sorted = apply(cbind(x_rc,x_rc_resamples),1,sort) - marg(res) # includes observed values + bias corrected (shifted by mean residual)
    
    ran_x = range(x)
    
    mcb_resamples = sapply(1:n_resamples,function(i) score(x,resamples[,i]) - score(x_rc_resamples[,i],resamples[,i]))
    mcb_bounds = sort(c(mcb,mcb_resamples))[c(low,up)]
    
    rank_obs = tail(rank(c(mcb_resamples,mcb)),1)
    pval = 1 - (rank_obs - 1)/(n_resamples + 1)
    
    lower = x_rc_resamples_sorted[low,]
    upper = x_rc_resamples_sorted[up,]
  }
  else{
    lower = NA
    upper = NA
    pval = NA
  }
  
  results <- data.frame(quantile = alpha, x = x, y = y, x_rc = x_rc,
                        lower = lower,upper = upper,
                        digits = digits,score = s,
                        umcb = umcb, cmcb = cmcb, mcb = mcb, dsc = dsc, unc = unc,
                        pval_cond = pval, pval_ucond = pval_ucond)
}



# Plot reliability diagram
plot.reldiag = function(df_reldiag, score_decomp = TRUE){
  if(score_decomp){
    digits = df_reldiag$digits[1]
    scores = df_reldiag %>%
      distinct(across(score:pval_ucond)) %>%
      mutate(label = paste0(c("\nuMCB ", "cMCB ", "DSC ", "UNC "),
                            format(round(c(umcb, cmcb, dsc, unc), digits = digits), nsmall = 1, trim = TRUE),
                            c(paste0(" [p = ", format(round(pval_ucond, digits = 2), nsmall = 2), "]"), "", "", ""),
                            c("", paste0(" [p = ", format(round(pval_cond, digits = 2), nsmall = 2), "]"), "", ""),
                            collapse = " \n"
      ))
    score_layer = list(
      geom_label(data = scores, mapping = aes(x = -Inf, y = Inf, label = sprintf(paste0("bar(S)~'%0.",digits,"f'"), score)),
                 size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA, alpha = 0, label.padding = unit(1, "lines"), parse = TRUE),
      geom_label(data = scores, mapping = aes(x = -Inf, y = Inf, label = label),
                 size = 6 * 0.36, hjust = 0, vjust = 1, label.size = NA, alpha = 0, label.padding = unit(1, "lines"), parse = FALSE)
      )
  }
  else score_layer = list()
  
  
  # needed to ensure square facets with equal x and y limits
  facet_lims <- df_reldiag %>%
    # group_by(model, quantile) %>%
    summarize(
      mn = min(c_across(c(x, x_rc, lower, upper))),
      mx = max(c_across(c(x, x_rc, lower, upper)))
    )
  
  ggplot(df_reldiag, aes(x, x_rc, group = model)) +
    # facet_grid(rows = vars(quantile), cols = vars(model)) +
    geom_point(aes(x, y), alpha = 0.3, size = 0.1) +
    geom_abline(intercept = 0, slope = 1, colour = "grey70") +
    geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
    geom_line(aes(x,lower), color = "deepskyblue2") +
    geom_line(aes(x,upper), color = "deepskyblue2") +
    geom_line(color = "firebrick3") +
    geom_blank(data = facet_lims, aes(x = mx, y = mx)) +
    geom_blank(data = facet_lims, aes(x = mn, y = mn)) +
    xlab("Forecast value") +
    ylab("Conditional quantile") +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(size = 0.05),
      panel.grid.minor = element_line(size = 0.05),
      strip.text.x = element_text(size = 7),
      strip.text.y = element_text(size = 7),
      aspect.ratio = 1
    ) +
    coord_fixed() +
    score_layer
}
