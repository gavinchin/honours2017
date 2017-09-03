## Alternative code for GLM
library(foreach)
library(doSNOW)
library(tcltk)

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


cl <- makeCluster(2)
registerDoSNOW(cl)

ped_models <- foreach(i = 1:43) %dopar% {
  model <- glm(Hourly_Counts ~ Month + DayType*Time, data = ped_list[[i]], family = quasipoisson())
  model <- stripGlmLR(model)
  return(model)
}
