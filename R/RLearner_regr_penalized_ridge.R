#' @S3method makeRLearner regr.penalized.ridge
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

#' @S3method trainLearner regr.penalized.ridge
trainLearner.regr.penalized.ridge = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  penalized(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner regr.penalized.ridge
predictLearner.regr.penalized.ridge = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data=.newdata,  ...)[,"mu"]
}
