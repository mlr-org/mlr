#' @export
makeRLearner.regr.penalized.lasso = function() {
  makeRLearnerRegr(
    cl = "regr.penalized.lasso",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda1", default = 0, lower = 0)
    ),
    properties = c("numerics", "factors"),
    name = "regr.penalized.lasso",
    short.name = "regr.penalized.lasso",
    note = ""
  )
}

#' @export
trainLearner.regr.penalized.lasso = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.penalized.lasso = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data = .newdata,  ...)[, "mu"]
}
