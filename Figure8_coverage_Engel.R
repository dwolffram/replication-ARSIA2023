source("models_Engel.R")

plot_coverage <- function(df, date_column = target_end_date, B = 1000, difference = FALSE, difference_OOS = FALSE, region = TRUE){
  type = "consistency"
  
  # Functions used
  coverage <- function(df){
    # df$l <- df$truth < floor(df$value)
    # df$u <- df$truth <= floor(df$value)
    
    eps = 10^-10 # avoid numerical artifacts...
    df$l <- df$truth < df$value - eps
    df$u <- df$truth < df$value + eps
    
    df <- df %>%
      group_by(model, type, quantile) %>%
      summarize(l = mean(l), u=mean(u), .groups = "drop")
    
    return(df)
  }
  
  # get_confidence_bands <- function(df, date_column, B = 100) {
  #   coverage_df <- data.frame()
  #   
  #   for (i in 1:B) {
  #     df_resampled <- df %>%
  #       group_by(!!date_column) %>%
  #       sample_n_groups(n = n_distinct(group_keys(.)), replace = TRUE)
  #     
  #     coverage_sample <- coverage(df_resampled)
  #     
  #     coverage_df <- bind_rows(coverage_df, coverage_sample)
  #   }
  #   
  #   # compute CIs from bootstrapped coverage
  #   coverage_df %>%
  #     group_by(model, quantile) %>%
  #     summarize(l_5 = quantile(l, 0.05),
  #               l_95 = quantile(l, 0.95),
  #               l_25 = quantile(l, 0.25),
  #               l_75 = quantile(l, 0.75),
  #               u_5 = quantile(u, 0.05),
  #               u_95 = quantile(u, 0.95),
  #               u_25 = quantile(u, 0.25),
  #               u_75 = quantile(u, 0.75),
  #               .groups = "drop")
  # }
  
  # get critical value of binomial test (used for consistency bands)
  # get_cr <- function(sample_size, alpha, nominal_level, alternative='less'){
  #   if(alternative == 'less'){
  #     C <- 0
  #     while(pbinom(C + 1, sample_size, alpha) < nominal_level) C <- C+1
  #   } 
  #   else if(alternative == 'greater'){
  #     C <- sample_size
  #     while (1 - pbinom(C-1, sample_size, alpha) < nominal_level) C <- C -1
  #   }
  #   return(C)
  # }
  
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
      group_by(model, type, quantile) %>%
      summarize(count = n(), .groups = "drop") %>%
      mutate(get_consistency_interval(quantile, count, 0.9) / count,
             get_consistency_interval(quantile, count, 0.5) / count) %>%
      select(-count)
  }
  
  
  # some customizations used in all plots
  my_theme <- list(
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = function(x) ifelse(x == 0, "0", x)),
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)),
    xlab("Quantile level"),
    ylab(NULL),
    theme_bw(base_size = 11),
    theme(panel.grid.major = element_line(size = 0.05), 
          panel.grid.minor = element_line(size = 0.05))
  )
  
  # compute coverage on full sample
  coverage_full <- coverage(df)
  
  # if (type == "confidence"){
  #   date_column <- enquo(date_column)
  #   dates <- df %>% distinct(!!date_column) %>% pull()
  #   
  #   # compute coverage on all bootstrap samples
  #   coverage_df = data.frame()  
  #   for(i in 1:B){
  #     dates_resampled <- sample(dates, replace = TRUE)
  #     
  #     # simple filter doesn't work because the same date can occur multiple times
  #     df_resampled <- lapply(dates_resampled, function(x){df %>% filter(target_end_date == x)}) %>%
  #       bind_rows()
  #     
  #     coverage_sample <- coverage(df_resampled)
  #     
  #     coverage_df <- bind_rows(coverage_df, coverage_sample)
  #   }
  #   
  #   # compute CIs from bootstrapped coverage
  #   results <- coverage_df %>%
  #     group_by(model, quantile, type) %>%
  #     summarize(l_5 = quantile(l, 0.05),
  #               l_95 = quantile(l, 0.95),
  #               l_25 = quantile(l, 0.25),
  #               l_75 = quantile(l, 0.75),
  #               u_5 = quantile(u, 0.05),
  #               u_95 = quantile(u, 0.95),
  #               u_25 = quantile(u, 0.25),
  #               u_75 = quantile(u, 0.75), 
  #               .groups = "drop")
  #   
  #   # compute coverage on full sample
  #   coverage_full <- coverage(df)
  #   
  #   results <- results %>%
  #     left_join(coverage_full, by = c("model", "quantile","type"))
  #   
  #   if (!difference){
  #     g <- ggplot(results) +
  #       facet_wrap(c("model","type"), ncol = 3) +
  #       geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size = 0.2, linetype = "solid", colour = "grey70")+
  #       geom_ribbon(aes(x = quantile, ymin = l_5, ymax = l_95), fill = "darkblue", alpha = 0.2) +
  #       geom_ribbon(aes(x = quantile, ymin = u_5, ymax = u_95), fill = "darkred", alpha = 0.2) +
  #       geom_errorbar(aes(x=quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
  #       my_theme +
  #       coord_fixed()
  #   }
  #   
  #   else{
  #     results <- results %>% 
  #       mutate_at(vars("l", "u", 
  #                      "l_5", "l_25", "l_75", "l_95", 
  #                      "u_5", "u_25","u_75", "u_95"), 
  #                 list(~ . - quantile))
  #     
  #     g <- ggplot(results) +
  #       facet_wrap("model") +
  #       geom_hline(yintercept = 0, size = 0.3, linetype = "solid", color = "darkgray") +
  #       geom_ribbon(aes(x = quantile, ymin = l_5, ymax = l_95), fill = "darkblue", alpha = 0.2) +
  #       geom_ribbon(aes(x = quantile, ymin = u_5, ymax = u_95), fill = "darkred", alpha = 0.2) +
  #       geom_errorbar(aes(x=quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
  #       my_theme
  #   }
  # } 
  # else
  if (type == "consistency"){
    # consistency_bands <- df %>% 
    #   group_by(model, type, quantile) %>% 
    #   summarize(count = n(), 
    #             .groups = "drop")
    # 
    # consistency_bands <- consistency_bands %>% 
    #   rowwise() %>% 
    #   mutate(lower90 = get_cr(count, quantile, 0.05, "less")/count,
    #          upper90 = get_cr(count, quantile, 0.05, "greater")/count,
    #          lower50 = get_cr(count, quantile, 0.25, "less")/count,
    #          upper50 = get_cr(count, quantile, 0.25, "greater")/count)
    # 
    # results <- results %>% 
    #   left_join(consistency_bands, by = c("model","type", "quantile"))
    
    bands <- get_consistency_bands(df)
    
    results <- coverage_full %>% left_join(bands, by = c("model", "type", "quantile"))
    
    if (!difference){
      results <- results %>% 
        mutate_at(vars("lower50", "upper50", "lower90", "upper90"), 
                  list(~ ifelse(type == is,quantile,.)))
      
      if(difference_OOS){
        results <- results %>%
          mutate_at(vars("l", "u", "lower50", "upper50", "lower90", "upper90"),
                    list(~ ifelse(type == is,.,. - quantile)))
        g = ggplot(results) +
          facet_grid(rows = vars(type), cols = vars(model),scales = "free_y")
      }
      else{
        g = ggplot(results) +
          facet_grid(rows = vars(type), cols = vars(model))
      }
      
      g <- g +
        geom_segment(aes(x = 0, xend = 1, y = 0, yend = ifelse(type != is & difference_OOS,0,1)), 
                     size = 0.2, linetype = "solid", colour = "grey70") +
        {if(region) geom_ribbon(aes(x = quantile, ymin = lower50, ymax = upper50), fill = "skyblue3", alpha = 0.3)} +
        {if(region) geom_ribbon(aes(x = quantile, ymin = lower90, ymax = upper90), fill = "skyblue3", alpha = 0.2)} +
        geom_errorbar(aes(x=quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
        my_theme +
        {if(!difference_OOS) ylab("Coverage")} +
        {if(difference_OOS) ylab("Coverage - Level                      Coverage     ")} +
        theme(aspect.ratio = 1)
    }
    
    else {
      results <- results %>% 
        mutate_at(vars("l", "u", "lower50", "upper50", "lower90", "upper90"), 
                  list(~ . - quantile))
      
      g <- ggplot(results) +
        facet_wrap("model","type") +
        geom_hline(yintercept = 0, size = 0.3, linetype = "solid", color = "darkgray") +
        {if(region) geom_ribbon(aes(x = quantile, ymin = lower50, ymax = upper50), fill = "skyblue3", alpha = 0.3)} +
        {if(region) geom_ribbon(aes(x = quantile, ymin = lower90, ymax = upper90), fill = "skyblue3", alpha = 0.2)} +
        geom_errorbar(aes(x=quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
        my_theme
      
    }
  }
  print(g)
  invisible(results)
}

plot_coverage(df = data_long,B = 100,difference_OOS = FALSE)
ggsave(filename = "figures/8_Engel_coverage.pdf",width = 160,height = 110,device = "pdf",units = "mm")
