#' @export
makeRLearner.regr.penalized.lasso = function() {
  makeRLearnerRegr(
    cl = "regr.penalized.lasso",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeDiscreteLearnerParam(id = "model", default = "linear",
        values = c("linear", "poisson")),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(trace = FALSE, model = "linear"),
    properties = c("numerics", "factors"),
    name = "Lasso Regression",
    short.name = "lasso",
    note = "trace=FALSE was set by default to disable logging output."
  )
}

#' @export
trainLearner.regr.penalized.lasso = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), fusedl = FALSE, ...)
}

#' @export
predictLearner.regr.penalized.lasso = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  # FIXME: should be removed, reported in issue 840
  m@formula$unpenalized[[2L]] = as.symbol(.model$task.desc$target)
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data = .newdata,  ...)[, "mu"]
}
