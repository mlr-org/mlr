#' @export
makeRLearner.classif.knn = function() {
  makeRLearnerClassif(
    cl = "classif.knn",
    package = "class",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "l", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "prob", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "use.all", default = TRUE)
    ),
    # knn cannot really return probs, only for the winning class (yeah well done BR)
    # knn also cannot handle factors in features apparantly
    properties = c("twoclass", "multiclass", "numerics"),
    name = "k-Nearest Neighbor",
    short.name = "knn",
    callees = "knn"
  )
}

#' @export
trainLearner.classif.knn = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, .subset, target.extra = TRUE)
  c(list(train = z$data, cl = z$target), list(...))
}

#' @export
predictLearner.classif.knn = function(.learner, .model, .newdata, ...) {
  args = .model$learner.model
  args$test = .newdata
  do.call(class::knn, args)
}
