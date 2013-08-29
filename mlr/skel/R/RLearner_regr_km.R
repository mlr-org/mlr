#' @S3method makeRLearner regr.km
makeRLearner.regr.km = function() {
  makeRLearnerRegr(
    cl = "regr.km",
    package = "DiceKriging",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="covtype", default="matern5_2", 
        values=list("gauss", "matern5_2", "matern3_2", "exp", "powexp")), 
      makeNumericLearnerParam(id="nugget"), 
      makeLogicalLearnerParam(id="nugget.estim", default=FALSE), 
      makeNumericVectorLearnerParam(id="noise.var"), 
      makeDiscreteLearnerParam(id="optim.method", default="BFGS", 
        values=list("BFGS", "gen")), 
      makeNumericVectorLearnerParam(id="lower"), 
      makeNumericVectorLearnerParam(id="upper"), 
      makeUntypedLearnerParam(id="control")
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    se = TRUE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.km
trainLearner.regr.km = function(.learner, .task, .subset, .weights,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  km(design=d$data, response=d$target, ...)
}

#' @S3method predictLearner regr.km
predictLearner.regr.km = function(.learner, .model, .newdata, ...) {
  se = (.learner$predict.type != "response")
  p = predict(.model$learner.model, newdata=.newdata, type="SK", se.compute=se)
  if(!se)
    return(p$mean)
  else
    cbind(p$mean, p$sd)
}