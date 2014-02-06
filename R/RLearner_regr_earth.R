#' @S3method makeRLearner regr.earth
makeRLearner.regr.earth = function() {
  makeRLearnerRegr(
    cl = "regr.earth",
    package = "earth",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="degree", default=1L, lower=1L),
      makeNumericLearnerParam(id="penalty"),
      makeIntegerLearnerParam(id="nprune")
      ),
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.earth
trainLearner.regr.earth = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  earth(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner regr.earth
predictLearner.regr.earth = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata)[, 1L]
}
