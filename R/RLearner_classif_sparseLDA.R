#' @export
makeRLearner.classif.sparseLDA = function() {
  makeRLearnerClassif(
    cl = "classif.sparseLDA",
    # FIXME: maybe again broken NAMESPACE / import in package, if we dont use !, solvebeta is not found
    package = c("!sparseLDA", "MASS", "!elasticnet"),
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", default = 1e-6, lower = 0, when = "train"),
      makeIntegerLearnerParam(id = "maxIte", default = 100L, lower = 0L, when = "train"),
      makeLogicalLearnerParam(id = "trace", default = FALSE, when = "train", tunable = FALSE),
      makeNumericLearnerParam(id = "tol", default = 1e-6, lower = 0, when = "train")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Sparse Discriminant Analysis",
    short.name = "sparseLDA",
    note = "Arguments `Q` and `stop` are not yet provided as they depend on the task.",
    callees = "sda"
  )
}

#' @export
trainLearner.classif.sparseLDA = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  y = d$target
  lvls = levels(y)
  y = sapply(lvls, function(lvl) as.integer(as.character(y) == lvl))
  sparseLDA::sda(x = d$data, y = y, ...)
}

#' @export
predictLearner.classif.sparseLDA = function(.learner, .model, .newdata, ...) {
  p = sparseLDA::predict.sda(.model$learner.model,
    newdata = subset(.newdata, select = .model$features), ...)
  if (.learner$predict.type == "response") {
    return(p$class)
  } else {
    return(p$posterior)
  }
}
