#' @export
makeRLearner.regr.randomUniformForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomUniformForest",
    package = "randomUniformForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeIntegerLearnerParam(id = "depth", lower = 3L),
      makeIntegerLearnerParam(id = "depthcontrol", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = FALSE),
      makeNumericLearnerParam(id = "subsamplerate", default = 0.7, lower = 0),
      makeLogicalLearnerParam(id = "bagging", default = FALSE),
      makeUntypedLearnerParam(id = "outputperturbationsampling", default = FALSE),
      makeDiscreteLearnerParam(id = "featureselectionrule", default = "L2", 
        values = c("random", "L2", "L1")),
      makeNumericVectorLearnerParam(id = "randomcombination", default = 0),
      makeLogicalLearnerParam(id = "randomfeature", default = FALSE),
      makeIntegerVectorLearnerParam(id = "categoricalvariablesidx"),
      makeDiscreteLearnerParam(id = "na.action", default = "fastImpute",
        values = c("fastImpute", "accurateImpute", "omit")),
      makeLogicalLearnerParam(id = "logX", default = FALSE),
      makeLogicalLearnerParam(id = "usesubtrees", default = FALSE),
      makeUntypedLearnerParam(id = "parallelpackage", default = "doParallel")
    ),
    properties = c("numerics", "factors", "ordered"),
    name = "Random Uniform Forest",
    short.name = "randomUniformForest"
    )
}

#' @export
trainLearner.regr.randomUniformForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  data = getTaskData(.task)
  #I set BreimanBounds to FALSE here, as recommended in the manual and we don't really need it right?
  randomUniformForest::randomUniformForest(formula = f, data = data, subset = .subset, regression = TRUE,
    OOB = FALSE, importance = FALSE, unsupervised = FALSE, threads = 1L, BreimanBounds = FALSE, ...)
}

#' @export
predictLearner.regr.randomUniformForest = function(.learner, .model, .newdata, ...) {
  predict(object = .model$learner.model, X = .newdata, threads = 1L, ...)
}