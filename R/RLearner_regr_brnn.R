#' @export
makeRLearner.regr.brnn = function() {
  makeRLearnerRegr(
    cl = "regr.brnn",
    package = "brnn",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "neurons", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "normalize", default = TRUE),
      makeIntegerLearnerParam(id = "epochs", default = 1000L, lower = 1L),
      makeNumericLearnerParam(id = "mu", default = 0.005, lower = .Machine$double.eps),
      makeNumericLearnerParam(id = "mu_dec", default = 0.1, lower = .Machine$double.eps),
      makeNumericLearnerParam(id = "mu_inc", default = 10, lower = .Machine$double.eps),
      makeNumericLearnerParam(id = "mu_max", default = 1e10, lower = .Machine$double.eps),
      makeNumericLearnerParam(id = "min_grad", default = 1e-10),
      makeNumericLearnerParam(id = "change", default = 0.001, lower = .Machine$double.eps),
      makeIntegerLearnerParam(id = "cores", default = 1L, lower = 1L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "Monte_Carlo", default = FALSE),
      makeNumericLearnerParam(id = "tol", default = 1e-06, lower = .Machine$double.eps),
      makeIntegerLearnerParam(id = "samples", default = 40L, lower = 1L),
      makeUntypedLearnerParam(id = "contrasts")
    ),
    properties = c("numerics", "factors"),
    name = "Bayesian regularization for feed-forward neural networks",
    short.name = "brnn",
    callees = "brnn"
  )
}

#' @export
trainLearner.regr.brnn = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  brnn::brnn(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.brnn = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}
