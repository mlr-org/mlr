#' @export
makeRLearner.surv.penalized.fusedlasso = function() {
  makeRLearnerSurv(
    cl = "surv.penalized.fusedlasso",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 1, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 1, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(lambda1 = 1, lambda2 = 1, trace = FALSE),
    properties = c("numerics", "factors", "ordered", "rcens"),
    name = "Fused Lasso Regression",
    short.name = "fusedlasso",
    note = "trace=FALSE was set by default to disable logging output. lambda1 and lambda2 have been set to 1 by default, as fusedlasso needs both penalizations > 0."
  )
}

#' @export
trainLearner.surv.penalized.fusedlasso = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, subset = .subset), model = "cox", fusedl = TRUE, ...)
}

#' @export
predictLearner.surv.penalized.fusedlasso = function(.learner, .model, .newdata, ...) {
  # Note: this is a rather ugly hack but should work according to Jelle
  penalized::survival(penalized::predict(.model$learner.model, data = .newdata), Inf)
}
