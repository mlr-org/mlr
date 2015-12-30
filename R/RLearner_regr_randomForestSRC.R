#' @export
makeRLearner.regr.randomForestSRC = function() {
  makeRLearnerRegr(
    cl = "regr.randomForestSRC",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none")),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 5L),
      makeIntegerLearnerParam(id = "nsplit", default = 0L),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute"), when = "both"),
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeNumericVectorLearnerParam(id = "xwar.wt", lower = 0),
      makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "fast.restore", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors"),
    name = "Random Forest",
    short.name = "rfsrc",
    note = "`na.action` has been set to `na.impute` by default to allow missing data support."
  )
}

#' @export
trainLearner.regr.randomForestSRC = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  randomForestSRC::rfsrc(f, data = getTaskData(.task, .subset), importance = "none", proximity = FALSE, forest = TRUE, ...)
}

#' @export
predictLearner.regr.randomForestSRC = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, importance = "none", ...)
  # versison 2.0 of randomForestSRC returns an array here :(
  as.numeric(p$predicted)
}
