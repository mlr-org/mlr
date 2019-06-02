#' @export
makeRLearner.regr.nodeHarvest = function() {
  makeRLearnerRegr(
    cl = "regr.nodeHarvest",
    package = "nodeHarvest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "nodesize", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "nodes", default = 1000L, lower = 1L),
      makeIntegerLearnerParam(id = "maxinter", default = 2L, lower = 1L),
      makeDiscreteLearnerParam(id = "mode", default = "mean", values = c("mean", "outbag")),
      makeNumericLearnerParam(id = "lambda"),
      makeUntypedLearnerParam(id = "addto", default = NULL),
      makeLogicalLearnerParam(id = "onlyinter"),
      makeLogicalLearnerParam(id = "silent", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "biascorr", default = FALSE)
    ),
    properties = c("numerics", "factors"),
    name = "Node Harvest",
    short.name = "nodeHarvest",
    callees = "nodeHarvest"
  )
}

#' @export
trainLearner.regr.nodeHarvest = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  nodeHarvest::nodeHarvest(X = d$data, Y = d$target, ...)
}

#' @export
predictLearner.regr.nodeHarvest = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, .newdata, ...)
}
