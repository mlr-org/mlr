#' @export
makeRLearner.regr.penalized.ridge = function() {
  makeRLearnerRegr(
    cl = "regr.penalized.ridge",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeDiscreteLearnerParam(id = "model", default = "linear",
        values = c("linear", "poisson")),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      # FIXME: Parameter dependent default for maxiter:
      # default is 25 if lambda1 = 0, lambda2 > 0, infinite otherwise
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    mlr.defaults = list(trace = FALSE, model = "linear"),
    properties = c("numerics", "factors"),
    name = "Ridge Regression",
    short.name = "ridge",
    note = "trace=FALSE was set by default to disable logging output."
  )
}

#' @export
trainLearner.regr.penalized.ridge = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), fusedl = FALSE, ...)
}

#' @export
predictLearner.regr.penalized.ridge = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  # FIXME: should be removed, reported in issue 840
  m@formula$unpenalized[[2L]] = as.symbol(.model$task.desc$target)
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data = .newdata,  ...)[, "mu"]
}
