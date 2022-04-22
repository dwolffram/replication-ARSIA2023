library(tidyverse)
library(reshape2)
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


data(engel)

n = dim(engel)[1]
q_levels = c(0.1, 0.25, 0.5, 0.75, 0.90)
col_names = c("predictor","truth","model",q_levels)
model_id = list(rq = "Linear",rqlog = "Log-linear",iso = "Isotonic")
is = "In-sample"


# In-sample
# Linear quantile regression
rq_engel = quantreg::rq(foodexp ~ income, tau = q_levels, data = engel)
# summary(rq_engel)
data_rq_is = data.frame(predictor = engel$income,truth = engel$foodexp,model = model_id$rq, predict(rq_engel))
names(data_rq_is) = col_names
data_rq_is_long = melt(data_rq_is,id.vars=col_names[1:3],variable.name = "quantile")
# head(data_rq_is_long)
data_rq_is_long$quantile = as.numeric(as.character(data_rq_is_long$quantile))

# Linear quantile regression after log-transformation
rqlog_engel = quantreg::rq(log(foodexp) ~ log(income), tau = q_levels, data = engel)
# summary(rq_engel)
data_rqlog_is = data.frame(predictor = engel$income,truth = engel$foodexp,model = model_id$rqlog, exp(predict(rqlog_engel)))
names(data_rqlog_is) = col_names
data_rqlog_is_long = melt(data_rqlog_is,id.vars=col_names[1:3],variable.name = "quantile")
# head(data_rqlog_is_long)
data_rqlog_is_long$quantile = as.numeric(as.character(data_rqlog_is_long$quantile))

# Isotonic quantile regression
data_iso_is = data.frame(predictor = engel$income,truth = engel$foodexp,model = model_id$iso)
for(alpha in q_levels){
  data_iso_is[,dim(data_iso_is)[2]+1] = pava(engel$income,engel$foodexp,p = alpha)$x
}
names(data_iso_is) = col_names
data_iso_is_long = melt(data_iso_is,id.vars=col_names[1:3],variable.name = "quantile")
# head(data_iso_is_long)
data_iso_is_long$quantile = as.numeric(as.character(data_iso_is_long$quantile))

# IS evaluation data
data_is_long = rbind(data_rq_is_long,data_rqlog_is_long,data_iso_is_long)
data_is_long$model = factor(data_is_long$model,levels = model_id)


# Out-of-sample (leave-one-out cross-validation)
# Linear quantile regression
data_rq_os = data.frame(predictor = engel$income,truth = engel$foodexp,model = model_id$rq)
j = dim(data_rq_os)[2]+1:length(q_levels)
for(i in 1:n){
  rq_engel <- quantreg::rq(foodexp ~ income, tau = q_levels, data = engel[-i,])
  data_rq_os[i,j] = predict(rq_engel,newdata = engel[i,])
}
names(data_rq_os) = col_names
data_rq_os_long = melt(data_rq_os,id.vars=col_names[1:3],variable.name = "quantile")
# head(data_rq_os_long)
data_rq_os_long$quantile = as.numeric(as.character(data_rq_os_long$quantile))

# Linear quantile regression after log-transformation
data_rqlog_os = data.frame(predictor = engel$income,truth = engel$foodexp,model = model_id$rqlog)
j = dim(data_rqlog_os)[2]+1:length(q_levels)
for(i in 1:n){
  rqlog_engel <- quantreg::rq(log(foodexp) ~ log(income), tau = q_levels, data=engel[-i,])
  data_rqlog_os[i,j] = exp(predict(rqlog_engel,newdata = engel[i,]))
}
names(data_rqlog_os) = col_names
data_rqlog_os_long = melt(data_rqlog_os,id.vars=col_names[1:3],variable.name = "quantile")
# head(data_rqlog_os_long)
data_rqlog_os_long$quantile = as.numeric(as.character(data_rqlog_os_long$quantile))

# Isotonic quantile regression
data_iso_os = data.frame(predictor = engel$income,truth = engel$foodexp,model = model_id$iso)
for(alpha in q_levels){
  j = dim(data_iso_os)[2]+1
  for(i in 1:n){
    model = pava(engel$income[-i],engel$foodexp[-i],p = alpha)
    data_iso_os[i,j] = predict.pava(model,engel$income[i])
  }
}
names(data_iso_os) = col_names
data_iso_os_long = melt(data_iso_os,id.vars=col_names[1:3],variable.name = "quantile")
# head(data_iso_os_long)
data_iso_os_long$quantile = as.numeric(as.character(data_iso_os_long$quantile))

# OoS evaluation data
data_os_long = rbind(data_rq_os_long,data_rqlog_os_long,data_iso_os_long)
data_os_long$model = factor(data_os_long$model,levels = model_id)

data_long = rbind(cbind(data_is_long,type = is),cbind(data_os_long,type = "Out-of-sample"))

