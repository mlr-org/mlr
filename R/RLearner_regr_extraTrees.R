#' @export
makeRLearner.regr.extraTrees = function() {
  makeRLearnerRegr(
    cl = "regr.extraTrees",
    package = "extraTrees",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 5L),
      makeIntegerLearnerParam(id = "numRandomCuts", default = 1L),
      makeLogicalLearnerParam(id = "evenCuts", default = FALSE),
      makeIntegerLearnerParam(id = "numThreads", default = 1L, lower = 1L),
      makeIntegerVectorLearnerParam(id = "subsetSizes"),
      makeUntypedLearnerParam(id = "subsetGroups"),
      makeIntegerVectorLearnerParam(id = "tasks"),
      makeNumericLearnerParam(id = "probOfTaskCuts", lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "numRandomTaskCuts", default = 1L, lower = 1L),
      makeDiscreteLearnerParam(id = "na.action", default = "stop",
        values = c("stop", "zero", "fuse"))
    ),
    properties = c("numerics", "weights"),
    name = "Extremely Randomized Trees",
    short.name = "extraTrees",
    callees = "extraTrees"
  )
}

#' @export
trainLearner.regr.extraTrees = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  args = c(list(x = as.matrix(d$data), y = d$target), list(...))
  if (!is.null(.weights)) {
    args$weights = .weights
  }
  do.call(extraTrees::extraTrees, args)
}

#' @export
predictLearner.regr.extraTrees = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, as.matrix(.newdata), ...)
}
