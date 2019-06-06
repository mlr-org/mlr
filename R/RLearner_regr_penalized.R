#' @export
makeRLearner.regr.penalized = function() {
  makeRLearnerRegr(
    cl = "regr.penalized",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "fusedl", default = FALSE),
      makeUntypedLearnerParam(id = "unpenalized", tunable = FALSE),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeDiscreteLearnerParam(id = "model", default = "linear",
        values = c("linear", "poisson")),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L, tunable = FALSE),
      # FIXME: Parameter dependent default for maxiter:
      # default is 25 if lambda1 = 0, lambda2 > 0, infinite otherwise
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(),
    properties = c("numerics", "factors"),
    name = "Penalized Regression",
    short.name = "penalized",
    note = "trace=FALSE was set by default to disable logging output.",
    callees = "penalized"
  )
}

#' @export
trainLearner.regr.penalized = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.penalized = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  # FIXME: should be removed, reported in issue 840
  m@formula$unpenalized[[2L]] = as.symbol(.model$task.desc$target)
  .newdata[, .model$task.desc$target] = 0
  penalized::predict(m, data = .newdata, ...)[, "mu"]
}
