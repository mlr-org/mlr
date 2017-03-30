#' @export
makeRLearner.surv.penalized = function() {
  makeRLearnerSurv(
    cl = "surv.penalized",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "fusedl", default = FALSE),
      makeUntypedLearnerParam(id = "unpenalized", tunable = FALSE),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L, tunable = FALSE),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE)
    ),
    par.vals = list(),
    properties = c("numerics", "factors", "ordered", "rcens"),
    name = "Penalized Cox Regression",
    short.name = "penalized",
    note = "trace=FALSE was set by default to disable logging output.",
    callees = "penalized"
  )
}

#' @export
trainLearner.surv.penalized = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, subset = .subset), model = "cox", ...)
}

#' @export
predictLearner.surv.penalized = function(.learner, .model, .newdata, ...) {
  # Note: this is a rather ugly hack but should work according to Jelle
  penalized::survival(penalized::predict(.model$learner.model, data = .newdata), Inf)
}
