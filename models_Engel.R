library(tidyverse)
library(quantreg)
library(isotone)


pava = function(z,y,p){
  # In case of ties, isotone::gpava uses the conditional mean instead of quantile, try e.g.,
  # gpava(c(-1,-1,-1),c(-1,0,0),solver = weighted.median,ties = "secondary")
  
  # Fix: Use ranking of predictor values and break ties by ordering the corresponding instances in order of decreasing observations
  ranking = match(1:length(z),order(z,y,decreasing = c(FALSE,TRUE)))
  model = gpava(ranking,y,solver = weighted.fractile,p = p,ties = "secondary")
  model$z = z
  return(model)
}

predict.pava = function(model,new_z){
  if(length(new_z)!=1) stop("Only handles one new predictor value at a time...")
  n = length(model$z)
  ord = order(model$z)
  ind = sum(model$z <= new_z)
  if(ind > 0 && ind < n){
    coeff = (new_z - model$z[ord][ind])/(model$z[ord][ind+1] - model$z[ord][ind])
    pred = (1-coeff)*model$x[ord][ind] + coeff*model$x[ord][ind+1]
  }
  else{
    if(ind == 0) pred = model$x[ord][1]
    else pred = model$x[ord][n]
  }
  return(pred)
}


data(engel, package = "quantreg")

n <- nrow(engel)
q_levels <- c(0.1, 0.25, 0.5, 0.75, 0.90)
col_names <- c("predictor", "truth", "model", q_levels)
model_id <- list(rq = "Linear", rqlog = "Log-linear", iso = "Isotonic")
ins <- "In-sample"


# In-sample
# Linear quantile regression
rq_engel <- quantreg::rq(foodexp ~ income, tau = q_levels, data = engel)
data_rq_ins <- data.frame(
  predictor = engel$income,
  truth = engel$foodexp,
  model = model_id$rq,
  predict(rq_engel)
)
names(data_rq_ins) <- col_names
data_rq_ins_long <- pivot_longer(data = data_rq_ins, col = starts_with("0"),
                                 names_to = "qlevel", 
                                 names_transform = list(qlevel = as.numeric))

# Linear quantile regression after log-transformation
rqlog_engel <- quantreg::rq(log(foodexp) ~ log(income), tau = q_levels, data = engel)
data_rqlog_ins = data.frame(
  predictor = engel$income,
  truth = engel$foodexp,
  model = model_id$rqlog,
  exp(predict(rqlog_engel))
)
names(data_rqlog_ins) <- col_names
data_rqlog_ins_long <- pivot_longer(data = data_rqlog_ins, col = starts_with("0"),
                                    names_to = "qlevel",
                                    names_transform = list(qlevel = as.numeric))

# Isotonic quantile regression
data_iso_ins = data.frame(
  predictor = engel$income,
  truth = engel$foodexp,
  model = model_id$iso
)
for (alpha in q_levels) {
  data_iso_ins[, ncol(data_iso_ins) + 1] <- pava(engel$income, engel$foodexp, p = alpha)$x
}
names(data_iso_ins) = col_names
data_iso_ins_long <- pivot_longer(data = data_iso_ins, col = starts_with("0"),
                                  names_to = "qlevel",
                                  names_transform = list(qlevel = as.numeric))

# InS evaluation data
data_ins_long <- rbind(data_rq_ins_long, data_rqlog_ins_long, data_iso_ins_long)
data_ins_long$model <- factor(data_ins_long$model, levels = model_id)


# Out-of-sample (leave-one-out cross-validation)
# Linear quantile regression
data_rq_oos <- data.frame(
  predictor = engel$income,
  truth = engel$foodexp,
  model = model_id$rq
)
j <- ncol(data_rq_oos) + 1:length(q_levels)
for (i in 1:n) {
  rq_engel <- quantreg::rq(foodexp ~ income, tau = q_levels, data = engel[-i, ])
  data_rq_oos[i, j] <- predict(rq_engel,newdata = engel[i, ])
}
names(data_rq_oos) <- col_names
data_rq_oos_long <- pivot_longer(data = data_rq_oos, col = starts_with("0"),
                                 names_to = "qlevel",
                                 names_transform = list(qlevel = as.numeric))

# Linear quantile regression after log-transformation
data_rqlog_oos <- data.frame(
  predictor = engel$income,
  truth = engel$foodexp,
  model = model_id$rqlog
)
j <- ncol(data_rqlog_oos) + 1:length(q_levels)
for (i in 1:n) {
  rqlog_engel <- quantreg::rq(log(foodexp) ~ log(income), tau = q_levels, data = engel[-i, ])
  data_rqlog_oos[i, j] <- exp(predict(rqlog_engel, newdata = engel[i, ]))
}
names(data_rqlog_oos) <- col_names
data_rqlog_oos_long <- pivot_longer(data = data_rqlog_oos, col = starts_with("0"),
                                    names_to = "qlevel",
                                    names_transform = list(qlevel = as.numeric))

# Isotonic quantile regression
data_iso_oos <- data.frame(
  predictor = engel$income,
  truth = engel$foodexp,
  model = model_id$iso
)
for (alpha in q_levels) {
  j <- ncol(data_iso_oos) + 1
  for (i in 1:n) {
    model <- pava(engel$income[-i], engel$foodexp[-i], p = alpha)
    data_iso_oos[i, j] = predict.pava(model, engel$income[i])
  }
}
names(data_iso_oos) <- col_names
data_iso_oos_long <- pivot_longer(data = data_iso_oos, col = starts_with("0"),
                                  names_to = "qlevel",
                                  names_transform = list(qlevel = as.numeric))

# OoS evaluation data
data_oos_long <- rbind(data_rq_oos_long, data_rqlog_oos_long, data_iso_oos_long)
data_oos_long$model <- factor(data_oos_long$model, levels = model_id)

data_long <- rbind(cbind(data_ins_long, type = ins),
                   cbind(data_oos_long, type = "Out-of-sample"))

rm(list = c(
  "rq_engel", "rqlog_engel", "data_oos_long", "i", "j",
  ls()[grepl("data_iso", ls()) | grepl("data_rq", ls()) |
         grepl("data_rqlog", ls())]))
