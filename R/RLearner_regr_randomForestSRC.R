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
      makeDiscreteLearnerParam(id = "na.action", values = c("na.omit", "na.impute"), default = "na.impute", when = "both"),
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeNumericVectorLearnerParam(id = "xwar.wt", lower = 0)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors"),
    name = "Random Forest",
    short.name = "rfsrc",
    note = "na.action' has been set to 'na.impute' by default to allow missing data support"
  )
}

#' @export
trainLearner.regr.randomForestSRC = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  randomForestSRC::rfsrc(f, data = getTaskData(.task, .subset), importance = "none", proximity = FALSE, forest = TRUE, ...)
}

#' @export
predictLearner.regr.randomForestSRC = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, importance = "none", ...)$predicted
}
