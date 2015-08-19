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
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "prob", "class.weights", "submodel"),
    class.weights.param = "classwt",
    submodel.param = "ntree",
    name = "Random Forest",
    short.name = "rf"
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
  type = ifelse(.learner$predict.type == "response", "response", "prob")
  dots = list(...)
  if ("submodel.value" %in% names(dots))
    .model$learner.model = extractSubforest(
      model = .model$learner.model, submodel.value = dots$submodel.value)
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

# reduces the size of the current randomForest to a value defined via submodel.value
extractSubforest = function(model, submodel.value) {
  rf = model
  ntree = submodel.value
  if (ntree > rf$ntree)
    stopf("ntree (%i) exceeds the number of trees in the forest (%i).", ntree, rf$ntree)
  rf$ntree = ntree
  rf$forest$ndbigtree = rf$forest$ndbigtree[seq_len(ntree), drop = FALSE]
  rf$forest$nodestatus = rf$forest$nodestatus[, seq_len(ntree), drop = FALSE]
  rf$forest$bestvar = rf$forest$bestvar[, seq_len(ntree), drop = FALSE]
  rf$forest$treemap = rf$forest$treemap[, , seq_len(ntree), drop = FALSE]
  rf$forest$nodepred = rf$forest$nodepred[, seq_len(ntree), drop = FALSE]
  rf$forest$xbestsplit = rf$forest$xbestsplit[, seq_len(ntree), drop = FALSE]
  rf$forest$ntree = ntree
  rf$err.rate = NULL
  rf$confusion = NULL
  rf$oob.times = NULL
  rf$votes = NULL
  rf$importanceSD = NULL
  return(rf)
}
