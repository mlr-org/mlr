#' @export
makeRLearner.regr.earth = function() {
  makeRLearnerRegr(
    cl = "regr.earth",
    package = "earth",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "degree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "penalty"),
      makeIntegerLearnerParam(id = "nprune")
      ),
    properties = c("numerics", "factors")
  )
}

#' @export
trainLearner.regr.earth = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  earth(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.earth = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata)[, 1L]
}
