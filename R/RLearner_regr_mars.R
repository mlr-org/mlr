#' @S3method makeRLearner regr.mars
makeRLearner.regr.mars = function() {
  makeRLearnerRegr(
    cl = "regr.mars",
    package = "mda",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="degree", default=1L, lower=1L),
      makeIntegerLearnerParam(id="nk", lower=1L),
      makeNumericLearnerParam(id="penalty", default=2, lower=0),
      makeNumericLearnerParam(id="thresh", default=0.001, lower=0),
      makeLogicalLearnerParam(id="prune", default=TRUE),
      makeLogicalLearnerParam(id="forward.step", default=TRUE)
    ),
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    se = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.mars
trainLearner.regr.mars = function(.learner, .task, .subset, .weights,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  mars(x = as.matrix(d$data), y = d$target, ...)
}

#' @S3method predictLearner regr.mars
predictLearner.regr.mars = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata)[, 1L]
}
