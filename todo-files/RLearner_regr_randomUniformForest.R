#' @export
makeRLearner.regr.randomUniformForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomUniformForest",
    package = "randomUniformForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeIntegerLearnerParam(id = "depth", lower = 1L),
      makeIntegerLearnerParam(id = "depthcontrol", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = FALSE),
      makeNumericLearnerParam(id = "subsamplerate", 0.7),
      makeLogicalLearnerParam(id = "bagging", default = FALSE),
      makeLogicalLearnerParam(id = "outputperturbationsampling", default = FALSE),
      makeDiscreteLearnerParam(id = "featureselectionrule", values = c("random", "L2", "L1")),
      makeNumericVectorLearnerParam(id = "randomcombination"),
      makeLogicalLearnerParam(id = "randomfeature", default = FALSE),
      makeLogicalLearnerParam(id = "logX", default = FALSE)
    ),
    properties = c("numerics", "factors", "ordered"),
    name = "Random Uniform Forests",
    short.name = "randomUniformForest",
    note = ""
  )
}

#' @export
trainLearner.regr.randomUniformForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  randomUniformForest::randomUniformForest(formula = f, data = getTaskData(.task, .subset), OOB = FALSE,
    importance = FALSE, unsupervised = FALSE, threads = 1L, ...)
}

#' @export
predictLearner.regr.randomUniformForest = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, .newdata, threads = 1L, ...)
}
