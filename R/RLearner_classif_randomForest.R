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
      makeLogicalLearnerParam(id = "norm.votes", default = TRUE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE),
      makeLogicalLearnerParam(id = "fix.factors", default = FALSE)
    ),
    par.vals = list(fix.factors = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob")
  )
}

#' @export
trainLearner.classif.randomForest = function(.learner, .task, .subset, .weights = NULL, classwt = NULL, cutoff, ...) {
  f = getTaskFormula(.task)
  levs = .task$task.desc$class.levels
  n = length(levs)
  if (missing(cutoff))
    cutoff = rep(1/n, n)
  if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt)))
    names(classwt) = levs
  if (is.numeric(cutoff) && length(cutoff) == n && is.null(names(cutoff)))
    names(cutoff) = levs
  randomForest(f, data = getTaskData(.task, .subset), classwt = classwt, cutoff = cutoff, ...)
}

#' @export
predictLearner.classif.randomForest = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type=="response", "response", "prob")
  if (.learner$par.vals$fix.factors) {
    factors = Filter(is.character, .model$learner.model$forest$xlevels)
    .newdata[names(factors)] = mapply(factor, x = .newdata[names(factors)],
       levels = factors, SIMPLIFY = FALSE)
  }
  predict(.model$learner.model, newdata=.newdata, type = type, ...)
}
