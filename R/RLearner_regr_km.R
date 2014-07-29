#' @export
makeRLearner.regr.km = function() {
  makeRLearnerRegr(
    cl = "regr.km",
    package = "DiceKriging",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "covtype", default = "matern5_2",
        values = list("gauss", "matern5_2", "matern3_2", "exp", "powexp")),
      makeNumericLearnerParam(id = "nugget"),
      makeLogicalLearnerParam(id = "nugget.estim", default = FALSE),
      makeNumericVectorLearnerParam(id = "noise.var"),
      makeDiscreteLearnerParam(id = "optim.method", default = "BFGS",
        values = list("BFGS", "gen")),
      makeNumericVectorLearnerParam(id = "lower"),
      makeNumericVectorLearnerParam(id = "upper"),
      makeUntypedLearnerParam(id = "control"),
      makeLogicalLearnerParam(id = "jitter", default = FALSE, when = "predict")
    ),
    par.vals = list(jitter = FALSE),
    properties = c("numerics", "se"),
    name = "Kriging",
    short.name = "km",
    note = ""
  )
}

#' @export
trainLearner.regr.km = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  km(design = d$data, response = d$target, ...)
}

#' @export
predictLearner.regr.km = function(.learner, .model, .newdata, jitter, ...) {
  # this is a bit stupid. km with nugget estim seems to perfectly interpolate the data
  # ONLY at exactly the training points
  # so we add minimal, numerical jitter to the x points
  if (jitter) {
    jit = matrix(rnorm(nrow(.newdata) * ncol(.newdata), mean = 0, sd = 1e-12), nrow = nrow(.newdata))
    .newdata = .newdata + jit
  }
  se = (.learner$predict.type != "response")
  p = predict(.model$learner.model, newdata = .newdata, type = "SK", se.compute = se)
  if(!se)
    return(p$mean)
  else
    cbind(p$mean, p$sd)
}
