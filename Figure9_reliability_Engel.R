source("models_Engel.R")

plot_relDiags = function(data_long,no_region = NULL,plot_hist = FALSE,log_xy = FALSE,insample = FALSE, digits = 1){
  reldiag = function(x, y, alpha = 0.5, n_resamples = 999, region_level = 0.9,
                     resample_log = TRUE){
    
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
    
    results <- data.frame(quantile = alpha, x = x, y = y, x_rc = x_rc,
                          lower = x_rc_resamples_sorted[low,],
                          upper = x_rc_resamples_sorted[up,],
                          score = s,
                          umcb = umcb, cmcb = cmcb, mcb = mcb, dsc = dsc, unc = unc,
                          pval_cond = pval, pval_ucond = pval_ucond)
  }
  
  get_inset <- function(df, xmin=0, xmax=0, ...){
    ggplot(df, aes(x)) +
      geom_histogram(fill="gray", col="black", size=0.2, bins = 8) +
      theme_classic( base_size=5.5) +
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())+
      expand_limits(x = c(xmin, xmax)) +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) 
  }
  
  annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax))
  }
  
  get_annotation <- function(df, model, xmax){
    inset_plot <- get_inset(df, xmax=xmax)
    annotation_custom2(grob=ggplotGrob(inset_plot), 
                       data = subset(df, model == unique(df$model)),
                       ymin = min(facet_lims$mn), ymax=max(facet_lims$mx)/4, xmin=max(facet_lims$mx)/1.5, xmax=0.975*max(facet_lims$mx))
    #ymin = 0, ymax=750, xmin=1500, xmax=2750)
    #ymin = -2000, ymax=4000, xmin=10000, xmax=15000)
  }
  
  
  scores = list()
  for(i in 1:length(q_levels)){
    quantile = q_levels[i]
    n_resamples = 999
    
    models = model_id
    
    df <- data_long %>%
      filter(model %in% models,
             # target == !!target,
             quantile == !!quantile)
    
    # compute recalibration, consistency band and score decomposition
    results <- df %>%
      group_by(model,type) %>%
      do(reldiag(.$value, .$truth, alpha = quantile, n_resamples = n_resamples))
    
    results <- results %>% 
      mutate_at(c("x_rc", "lower", "upper"), ~ replace(., .<0, 0)) %>%
      mutate_at(c("lower", "upper"), ~ replace(., is.element(model,no_region) && type == is, x_rc))
    
    # summarize scores and create labels
    scores[[i]] <- results %>%
      group_by(model,type) %>%
      distinct(across(score:pval_ucond)) %>%
      mutate(quantile = quantile) %>%
      mutate(label = paste0(c("\nuMCB ","cMCB ","DSC ","UNC "),
                            c(ifelse(type == is,format(round(umcb,digits = digits), nsmall = 0),
                                     format(round(umcb,digits = digits),nsmall = digits)),
                              ifelse(type == is & model == model_id[3],format(round(cmcb,digits = digits), nsmall = 0),
                                     format(round(cmcb,digits = digits),nsmall = digits)),
                              format(round(c(dsc,unc),digits = digits),nsmall = digits)),
                            # round(c(umcb, cmcb, dsc, unc), digits = digits),
                            c(ifelse(type == is,"",paste0(" [p = ", round(pval_ucond, digits = 2),"]")), "", "", ""),
                            c("", ifelse(type == is & model == model_id[3],"",paste0(" [p = ", round(pval_cond, digits = 2),"]")), "", ""),
                            collapse = "\n"))
    
    # cols <- c("x", "y", "x_rc", "lower", "upper")
    # results[cols] <- sqrt(results[cols])
    
    # needed to ensure square facets with equal x and y limits
    facet_lims <- results %>%
      group_by(model) %>%
      summarize(mn = min(c_across(c(x, x_rc, lower, upper))), 
                mx = max(c_across(c(x, x_rc, lower, upper))))
    
    main_plot <- ggplot(results, aes(x, x_rc, group=model)) +
      # facet_wrap(~model, ncol=3) +
      facet_grid(rows = vars(type), cols = vars(model)) +
      {if(!plot_hist) geom_point(aes(x, y), alpha=0.2,size = 0.6)} +
      geom_abline(intercept = 0 , slope = 1, colour="grey70") +
      # geom_point(color = "red", size=0.5) +
      # geom_step(color = "red", direction = "vh") +    
      geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
      {if(!plot_hist) geom_line(aes(x,lower), color = "deepskyblue2")} +
      {if(!plot_hist) geom_line(aes(x,upper), color = "deepskyblue2")} +
      geom_line(color = "firebrick3") +
      # geom_rug(sides = "b", alpha = 0.2, size = 0.25) +
      geom_blank(data = facet_lims, aes(x = mx, y = mx)) +
      geom_blank(data = facet_lims, aes(x = mn, y = mn)) +
      xlab("Forecast value") +
      ylab("Conditional quantile") +
      # labs(title = paste0(model, ":\n", target))  +
      geom_label(data = scores[[i]], mapping = aes(x = ifelse(log_xy,0,-Inf), y = Inf, label = label),# parse = TRUE,
                 size = 6*0.36, hjust = 0, vjust = 1, label.size = NA, alpha=0, label.padding = unit(1, "lines")) +
      geom_label(data = scores[[i]], mapping = aes(x = ifelse(log_xy,0,-Inf), y = Inf, label = sprintf("bar(S)~'%0.1f'",score)),parse = TRUE,
                 size = 6*0.36, hjust = 0, vjust = 1, label.size = NA, alpha=0, label.padding = unit(1, "lines")) +
      {if(!log_xy) scale_x_continuous(guide = guide_axis(check.overlap = TRUE))} +
      {if(log_xy) scale_x_log10(guide = guide_axis(check.overlap = TRUE))} + 
      {if(log_xy) scale_y_log10()} +
      # geom_histogram(mapping = aes(x = x,y = 0.2*max(facet_lims$mx)*after_stat(count/max(count))),bins = 10,colour = "grey", fill = NA) +
      theme_bw(base_size = 11) +
      theme(panel.grid.major = element_line(size = 0.05), 
            panel.grid.minor = element_line(size = 0.05)) +
      coord_fixed()
    
    
    if(plot_hist){
      insets <- results %>%
        group_by(model) %>%
        group_map(get_annotation, xmax=max(facet_lims$mx), .keep=TRUE)
      
      main_plot = main_plot + insets
    }
    
    plot(main_plot)
    
    # readline(prompt = paste0("Reliability Diagram at level ",quantile,". Press [enter] to continue."))
  }
  return(scores)
}

width = 160/25.4
height = 110/25.4
set.seed(100)

pdf("figures/9_Engel_reliability.pdf",width = width,height = height)
scores = plot_relDiags(data_long,no_region = "Isotonic",log_xy = TRUE,insample = TRUE)
dev.off()

# Table 1
library(xtable)

scores = rbind(select(do.call(rbind,scores),
                      -c(label,pval_cond,pval_ucond)))
scores = melt(scores,id.vars = c("model","quantile","type")) %>% dcast(formula = quantile + variable ~ model + type,mean)
scores = scores[c("quantile", "variable","Linear_In-sample","Log-linear_In-sample","Isotonic_In-sample",
                  "Linear_Out-of-sample","Log-linear_Out-of-sample","Isotonic_Out-of-sample")]
print(xtable(scores,align = rep("c",9),digits = 1),include.rownames = FALSE)


