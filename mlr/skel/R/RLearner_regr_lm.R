#' @S3method makeRLearner regr.lm
makeRLearner.regr.lm = function() {
  makeRLearnerRegr(
    cl = "regr.lm",
    package = "stats",
    par.set = makeParamSet(
  			 makeDiscreteLearnerParam(id="method", default="moment", values=c("moment", "mle", "mve", "t")),
  			 makeNumericLearnerParam(id="nu", lower=2, requires=expression(method=="t")),
      makeNumericLearnerParam(id="tol", default=1.0e-4, lower=0)
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = TRUE,
    weights = TRUE
  )
}
		
#' @S3method trainLearner regr.lm
trainLearner.regr.lm = function(.learner, .task, .subset, .weights,  ...) {
  d = getTaskData(.task, .subset)
  if (missing(.weights)) {
    f = getTaskFormula(.task)
    lm(f, data=d, ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    lm(f, data=d, weights=.weights, ...)
  }
}
	
#' @S3method predictLearner regr.lm
predictLearner.regr.lm = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata=.newdata, se.fit=FALSE, ...)
  } else {
    p = predict(.model$learner.model, newdata=.newdata, se.fit=TRUE, ...)
    cbind(p$fit, p$se.fit)
  }
}
