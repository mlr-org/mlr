#' @export
makeRLearner.classif.randomUniformForest = function() {
  makeRLearnerClassif(
    cl = "classif.randomUniformForest",
    package = "randomUniformForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeIntegerLearnerParam(id = "depth", lower = 3L),
      makeIntegerLearnerParam(id = "depthcontrol", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericLearnerParam(id = "subsamplerate", default = 1, lower = 0),
      makeLogicalLearnerParam(id = "bagging", default = FALSE),
      makeNumericVectorLearnerParam(id = "classwt", lower = 0),
      makeNumericLearnerParam(id = "oversampling", default = 0, lower = -1, upper = 1),
      makeNumericLearnerParam(id = "targetclass", default = -1),
      makeUntypedLearnerParam(id = "outputperturbationsampling", default = FALSE),
      makeLogicalLearnerParam(id = "rebalancedsampling", default = FALSE),
      makeDiscreteLearnerParam(id = "featureselectionrule", default = "entropy", 
        values = c("entropy", "gini", "random", "L2", "L1")),
      makeNumericVectorLearnerParam(id = "randomcombination", default = 0),
      makeLogicalLearnerParam(id = "randomfeature", default = FALSE),
      makeIntegerVectorLearnerParam(id = "categoricalvariablesidx"),
      makeDiscreteLearnerParam(id = "na.action", default = "fastImpute",
        values = c("fastImpute", "accurateImpute", "omit")),
      makeLogicalLearnerParam(id = "logX", default = FALSE),
      makeUntypedLearnerParam(id = "classcutoff", default = c(0, 0)),
      makeLogicalLearnerParam(id = "usesubtrees", default = FALSE),
      makeUntypedLearnerParam(id = "parallelpackage", default = "doParallel")
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "prob"),
    class.weights.param = "classwt",
    name = "Random Uniform Forest",
    short.name = "randomUniformForest"
    )
}

#' @export
trainLearner.classif.randomUniformForest = function(.learner, .task, .subset, .weights = NULL, classwt = NULL, ...) {
  f = getTaskFormula(.task)
  data = getTaskData(.task)
  #I set BreimanBounds to FALSE here, as recommended in the manual and we don't really need it right?
  randomUniformForest::randomUniformForest(formula = f, data = data, classwt = classwt, subset = .subset,
   regression = FALSE, OOB = FALSE, importance = FALSE, unsupervised = FALSE, threads = 1L, BreimanBounds = FALSE, ...)
}

#' @export
predictLearner.classif.randomUniformForest = function(.learner, .model, .newdata, ...) {
  predict(object = .model$learner.model, X = .newdata, type = .learner$predict.type, threads = 1L, ...)
}