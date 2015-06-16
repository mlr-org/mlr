#' @export
makeRLearner.regr.penalized.ridge = function() {
  makeRLearnerRegr(
    cl = "regr.penalized.ridge",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors"),
    name = "Penalized Ridge Regression",
    short.name = "ridge",
    note = ""
  )
}

#' @export
trainLearner.regr.penalized.ridge = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.penalized.ridge = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data = .newdata,  ...)[, "mu"]
}
