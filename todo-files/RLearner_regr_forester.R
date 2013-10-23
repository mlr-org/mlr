# FIXME
#makeRLearner.regr.kmforrester = function() {
#  makeRLearnerRegr(
#    cl = "regr.kmforrester",
#    package = "DiceKriging",
#    missings = FALSE,
#    numerics = TRUE,
#    factors = FALSE,
#    se = FALSE,
#    weights = FALSE
#  )
#}
#
#trainLearner.regr.kmforrester = function(.learner, .task, .subset,  ...) {
#  d = getTaskData(.task, .subset, target.extra=TRUE)
#  d = getTaskData(.task, .subset, target.extra=TRUE)
#  m = km(design=d$data, response=d$target, nugget.estim=TRUE, ...)
#  p = predict(m, d$data, type="SK")$mean
#  m = km(design=d$data, response=p, nugget.estim=FALSE, 
#    coef.trend=m$trend.coef, coef.var=m$covariance$sd2, coef.cov=m$covariance$range.val)
#  return(m)  
#}
#
#predictLearner.regr.kmforrester = function(.learner, .model, .newdata, ...) {
#  p = predict(.model$learner.model, newdata=.newdata, type="SK", se.compute=FALSE, ...)
#  return(p$mean) 
#}
