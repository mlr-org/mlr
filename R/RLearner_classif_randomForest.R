#' @export
makeRLearner.classif.randomForest = function() {
  makeRLearnerClassif(
    cl = "classif.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericVectorLearnerParam(id = "classwt", lower = 0),
      makeNumericVectorLearnerParam(id = "cutoff", lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "sampsize", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeLogicalLearnerParam(id = "importance", default = FALSE),
      makeLogicalLearnerParam(id = "localImp", default = FALSE),
      makeLogicalLearnerParam(id = "norm.votes", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "prob", "class.weights"),
    class.weights.param = "classwt",
    name = "Random Forest",
    short.name = "rf",
    note = "Note that the rf can freeze the R process if trained on a task with 1 feature which is constant. This can happen in feature forward selection, also due to resampling, and you need to remove such features with removeConstantFeatures."
  )
}

#' @export
trainLearner.classif.randomForest = function(.learner, .task, .subset, .weights = NULL, classwt = NULL, cutoff, ...) {
  f = getTaskFormula(.task)
  data = getTaskData(.task, .subset, recode.target = "drop.levels")
  levs = levels(data[,getTaskTargetNames(.task)])
  n = length(levs)
  if (missing(cutoff))
    cutoff = rep(1/n, n)
  if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt)))
    names(classwt) = levs
  if (is.numeric(cutoff) && length(cutoff) == n && is.null(names(cutoff)))
    names(cutoff) = levs
  randomForest::randomForest(f, data = data, classwt = classwt, cutoff = cutoff, ...)
}

#' @export
predictLearner.classif.randomForest = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type=="response", "response", "prob")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
