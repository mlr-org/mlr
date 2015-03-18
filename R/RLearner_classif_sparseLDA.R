#' @export
makeRLearner.classif.sparseLDA = function() {
  makeRLearnerClassif(
    cl = "classif.sparseLDA",
    package = "sparseLDA",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", default = 1e-6, lower = 0, when = "train"),
      makeIntegerLearnerParam(id = "maxIte", default = 100L, lower = 0L, when = "train"),
      makeLogicalLearnerParam(id = "trace", default = FALSE, when = "train"),
      makeNumericLearnerParam(id = "tol", default = 1e-6, lower = 0, when = "train")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Sparse Discriminant Analysis",
    short.name = "sparseLDA",
    note = "Arguments Q and stop are not yet provided as they depend on the task."
  )
}

#' @export
trainLearner.classif.sparseLDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  y = getTaskTargets(subsetTask(.task, .subset))
  lvls = levels(y)
  y = sapply(lvls, function(lvl) as.integer(as.character(y) == lvl))
  X = getTaskData(.task, .subset)
  X = subset(X, select = getTaskFeatureNames(.task))
  args = c(list(x = X, y = y), list(...))
  do.call(sparseLDA::sda, args)
}

#' @export
predictLearner.classif.sparseLDA = function(.learner, .model, .newdata, ...) {
  p = sparseLDA::predict.sda(.model$learner.model, newdata = subset(.newdata, select = .model$features), ...)
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}
