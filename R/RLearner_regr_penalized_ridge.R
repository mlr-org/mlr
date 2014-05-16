#' @export
makeRLearner.regr.penalized.ridge = function() {
  makeRLearnerRegr(
    cl = "regr.penalized.ridge",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="lambda2", default=0, lower=0)
    ),
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

#' @export
trainLearner.regr.penalized.ridge = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  penalized(f, data=getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.penalized.ridge = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data=.newdata,  ...)[, "mu"]
}
