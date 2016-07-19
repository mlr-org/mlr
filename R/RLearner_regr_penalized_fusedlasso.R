#' @export
makeRLearner.regr.penalized.fusedlasso = function() {
  makeRLearnerRegr(
    cl = "regr.penalized.fusedlasso",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 1, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 1, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeDiscreteLearnerParam(id = "model", default = "linear",
        values = c("linear", "poisson")),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      # FIXME: Parameter dependent default:
      # default is 25 if lambda1 = 0, lambda2 > 0, infinite otherwise
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    mlr.defaults = list(trace = FALSE, model = "linear", lambda1 = 1, lambda2 = 1),
    properties = c("numerics", "factors"),
    name = "Fused Lasso Regression",
    short.name = "fusedlasso",
    note = "trace=FALSE was set by default to disable logging output. lambda1 and lambda2 have been set to 1 by default, as fusedlasso needs both penalizations > 0."
  )
}

#' @export
trainLearner.regr.penalized.fusedlasso = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), fusedl = TRUE, ...)
}

#' @export
predictLearner.regr.penalized.fusedlasso = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  # FIXME: should be removed, reported in issue 840
  m@formula$unpenalized[[2L]] = as.symbol(.model$task.desc$target)
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data = .newdata,  ...)[, "mu"]
}
