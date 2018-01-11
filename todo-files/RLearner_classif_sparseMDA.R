#' @export
makeRLearner.classif.sparseMDA = function() {
  makeRLearnerClassif(
    cl = "classif.sparseMDA",
    package = c("sparseLDA", "MASS", "elasticnet"),
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", default = 1e-6, lower = 0, when = "train"),
      makeIntegerLearnerParam(id = "maxIte", default = 50L, lower = 0L, when = "train"),
      makeLogicalLearnerParam(id = "trace", default = FALSE, when = "train"),
      makeNumericLearnerParam(id = "tol", default = 1e-4, lower = 0, when = "train")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Sparse Mixture Discriminant Analysis",
    short.name = "sparseMDA",
    note = "Arguments Q and stop as well as Z and Rj are not yet provided."
  )
}

#' @export
trainLearner.classif.sparseMDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  y = d$target
  lvls = levels(y)
  y = sapply(lvls, function(lvl) as.integer(as.character(y) == lvl))
  sparseLDA::smda(x = d$data, y = y, ...)
}

#' @export
predictLearner.classif.sparseMDA = function(.learner, .model, .newdata, ...) {
  p = sparseLDA::predict.smda(.model$learner.model, 
    newdata = subset(.newdata, select = .model$features), ...)
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}
