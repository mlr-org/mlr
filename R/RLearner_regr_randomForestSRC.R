#' @export
makeRLearner.regr.randomForestSRC = function() {
  makeRLearnerRegr(
    cl = "regr.randomForestSRC",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", values = c("by.root", "by.node", "none"), default = "by.root"),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 5L),
      makeDiscreteLearnerParam(id = "na.action", values = c("na.omit", "na.impute"), default = "na.omit"),
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeNumericVectorLearnerParam(id = "xwar.wt", lower = 0)
    ),
    properties = c("missings", "numerics", "factors"),
    name = "regr.randomForestSRC",
    short.name = "regr.randomForestSRC",
    note = ""
  )
}

#' @export
trainLearner.regr.randomForestSRC = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  rfsrc(f, data = getTaskData(.task, .subset), importance = "none", proximity = FALSE, forest = TRUE, ...)
}

#' @export
predictLearner.regr.randomForestSRC = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, importance = "none", na.action = "na.impute", ...)$predicted
}
