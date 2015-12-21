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
      makeDiscreteLearnerParam(id = "estim.method", default = "MLE",
        values = c("MLE", "LOO")),
      makeDiscreteLearnerParam(id = "optim.method", default = "BFGS",
        values = c("BFGS", "gen")),
      makeNumericVectorLearnerParam(id = "lower"),
      makeNumericVectorLearnerParam(id = "upper"),
      makeUntypedLearnerParam(id = "control"),
      makeLogicalLearnerParam(id = "gr", default = TRUE),
      makeLogicalLearnerParam(id = "iso", default = FALSE),
      makeLogicalLearnerParam(id = "scaling", default = FALSE),
      makeLogicalLearnerParam(id = "jitter", default = FALSE, when = "predict")
    ),
    par.vals = list(jitter = FALSE),
    properties = c("numerics", "se"),
    name = "Kriging",
    short.name = "km",
    note = 'In predict, we currently always use `type = "SK"`. The extra parameter `jitter` (default is `FALSE`) enables adding a very small jitter (order 1e-12) to the x-values before prediction, as `predict.km` reproduces the exact y-values of the training data points, when you pass them in, even if the nugget effect is turned on.'
  )
}

#' @export
trainLearner.regr.km = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  DiceKriging::km(design = d$data, response = d$target, ...)
}

#' @export
predictLearner.regr.km = function(.learner, .model, .newdata, jitter, ...) {
  # km with nugget estim perfectly interpolate the datas ONLY at exactly the training points
  # see JSS paper for explanation
  # so we add minimal, numerical jitter to the x points
  if (jitter) {
    jit = matrix(rnorm(nrow(.newdata) * ncol(.newdata), mean = 0, sd = 1e-12), nrow = nrow(.newdata))
    .newdata = .newdata + jit
  }
  se = (.learner$predict.type != "response")
  p = DiceKriging::predict.km(.model$learner.model, newdata = .newdata, type = "SK", se.compute = se)
  if(!se)
    return(p$mean)
  else
    cbind(p$mean, p$sd)
}
