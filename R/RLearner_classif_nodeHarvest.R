#' @export
makeRLearner.classif.nodeHarvest = function() {
  makeRLearnerClassif(
    cl = "classif.nodeHarvest",
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
    properties = c("numerics", "factors", "twoclass", "prob"),
    name = "Node Harvest",
    short.name = "nodeHarvest",
    callees = "nodeHarvest"
  )
}

#' @export
trainLearner.classif.nodeHarvest = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "01")
  nodeHarvest::nodeHarvest(X = d$data, Y = d$target, ...)
}

#' @export
predictLearner.classif.nodeHarvest = function(.learner, .model, .newdata, ...) {
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  p = predict(.model$learner.model, .newdata, ...)
  if (.learner$predict.type == "prob") {
    p = setColNames(cbind(1 - p, p), levs)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
  }
  return(p)
}
