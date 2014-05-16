#' @export
makeRLearner.regr.penalized.lasso = function() {
  makeRLearnerRegr(
    cl = "regr.penalized.lasso",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="lambda1", default=0, lower=0)
    ),
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

#' @export
trainLearner.regr.penalized.lasso = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  penalized(f, data=getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.penalized.lasso = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data=.newdata,  ...)[, "mu"]
}
