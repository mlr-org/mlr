#' @export
makeRLearner.regr.kmforrester = function() {
 	makeRLearnerRegr(
   	cl = "regr.kmforrester",
   	package = "DiceKriging",
   	par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "covtype", default = "matern5_2",
        values = list("gauss", "matern5_2", "matern3_2", "exp", "powexp")),
      makeNumericVectorLearnerParam(id = "noise.var"),
      makeDiscreteLearnerParam(id = "optim.method", default = "BFGS",
        values = list("BFGS", "gen")),
      makeNumericVectorLearnerParam(id = "lower"),
      makeNumericVectorLearnerParam(id = "upper"),
      makeUntypedLearnerParam(id = "control")
    ),
    properties = c("numerics", "se"),
    name = "Kriging-Reinterpolation",
    short.name = "kmforrester",
    note = "In predict, we currently always use type = 'SK'."
 	)
}

#' @export
trainLearner.regr.kmforrester = function(.learner, .task, .subset,  ...) {
 	d = getTaskData(.task, .subset, target.extra = TRUE)
 	m = km(design = d$data, response = d$target, nugget.estim = TRUE, ...)
 	p = predict(m, d$data, type = "SK")$mean
 	m = km(design = d$data, response = p, nugget.estim = FALSE, 
   coef.trend = m@trend.coef, coef.var = m@covariance@sd2, coef.cov = m@covariance@range.val)
 	return(m)
}

#' @export
predictLearner.regr.kmforrester = function(.learner, .model, .newdata, ...) {
  se = (.learner$predict.type != "response")
  p = predict(.model$learner.model, newdata = .newdata, type = "SK", se.compute = se)
  if(!se)
    return(p$mean)
  else
    cbind(p$mean, p$sd)
}
