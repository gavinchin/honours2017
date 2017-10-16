## predictive model estimation
## requires imputation to have been already completed

library(dplyr)
library(pryr)

### code from:
## http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
stripGlmLR = function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}
#######################################

tr_df <- dfa2
locs <- loc$ids %>% as.character()

pred_model <- list()
  for(i in locs) {
  model <- glm(eval(as.name(i)) ~ Month + DayType*Time, data = tr_df,
               family = quasipoisson())
  pred_model[[i]] <- stripGlmLR(model)
}

save(pred_model, file = "data/model_est.RData")
