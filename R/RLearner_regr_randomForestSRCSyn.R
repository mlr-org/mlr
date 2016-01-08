#' @export
makeRLearner.regr.randomForestSRCSyn = function() {
  makeRLearnerRegr(
    cl = "regr.randomForestSRCSyn",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerVectorLearnerParam(id = "mtrySeq"),
      makeIntegerLearnerParam(id = "nodesize", default = 5L, lower = 1L),
      makeIntegerVectorLearnerParam(id = "nodesizeSeq", default = c(1L:10L, 20L, 30L, 50L, 100L)),
      makeNumericLearnerParam(id = "nsplit", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "min.node", default = 3L, ),
      makeLogicalLearnerParam(id = "use.org.features", default = TRUE),
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
    properties = c("numerics", "factors", "ordered"),
    name = "Synthetic Random Forest",
    short.name = "rfsrcSyn",
    note = "na.action' has been set to 'na.impute' by default to allow missing data support"
    )
}

#' @export
trainLearner.regr.randomForestSRCSyn = function(.learner, .task, .subset, .weights = NULL, ...) {
  ##using parameters object and newdata throws an error, so we need to train like this
  df = getTaskData(.task, .subset)
  f = getTaskFormula(.task)
  c(list(formula = f, data = df), list(importance = "none", proximity = FALSE, forest = TRUE, verbose = FALSE, ...))
}

#' @export
predictLearner.regr.randomForestSRCSyn = function(.learner, .model, .newdata, ...) {
  args = .model$learner.model
  args$newdata = .newdata
  args$verbose = FALSE
  p = do.call(randomForestSRC::rfsrcSyn, args)$rfSynPred$predicted
  # versison 2.0 of randomForestSRC returns an array here :(
  p = as.numeric(p)
  return(p)
}
