#' @export
makeRLearner.classif.randomUniformForest = function() {
  makeRLearnerClassif(
    cl = "classif.randomUniformForest",
    package = "randomUniformForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeIntegerLearnerParam(id = "depth", lower = 1L),
      makeIntegerLearnerParam(id = "depthcontrol", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeLogicalLearnerParam(id = "bagging", default = FALSE),
      makeNumericLearnerParam(id = "oversampling", default = 0, lower = -1, upper = 1),
      makeIntegerLearnerParam(id = "targetclass", default = -1),
      makeLogicalLearnerParam(id = "outputperturbationsampling", default = FALSE),
      makeUntypedLearnerParam(id = "rebalancedsampling"),
      makeDiscreteLearnerParam(id = "featureselectionrule", default = "entropy", values = c("random", "entropy", "gini")),
      makeNumericVectorLearnerParam(id = "randomcombination"),
      makeLogicalLearnerParam(id = "randomfeature", default = FALSE),
      makeLogicalLearnerParam(id = "logX", default = FALSE),
      makeUntypedLearnerParam(id = "classcutoff", when = "both")
    ),
    properties = c("numerics", "factors", "ordered", "prob", "twoclass", "multiclass"),
    name = "Random Uniform Forests",
    short.name = "randomUniformForest",
    note = ""
  )
}

#' @export
trainLearner.classif.randomUniformForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  randomUniformForest::randomUniformForest(formula = f, data = getTaskData(.task, .subset), OOB = FALSE,
    importance = FALSE, unsupervised = FALSE, threads = 1L, ...)
}

#' @export
predictLearner.classif.randomUniformForest = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, .newdata, type = .learner$predict.type, threads = 1L, ...)
}
