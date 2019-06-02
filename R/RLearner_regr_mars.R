#' @export
makeRLearner.regr.mars = function() {
  makeRLearnerRegr(
    cl = "regr.mars",
    package = "mda",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "degree", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "nk", lower = 1L),
      makeNumericLearnerParam(id = "penalty", default = 2, lower = 0),
      makeNumericLearnerParam(id = "thresh", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "prune", default = TRUE),
      makeLogicalLearnerParam(id = "trace.mars", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "forward.step", default = TRUE)
    ),
    properties = "numerics",
    name = "Multivariate Adaptive Regression Splines",
    short.name = "mars",
    callees = "mars"
  )
}

#' @export
trainLearner.regr.mars = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  mda::mars(x = as.matrix(d$data), y = d$target, ...)
}

#' @export
predictLearner.regr.mars = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata)[, 1L]
}
