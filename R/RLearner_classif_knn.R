#' @export
makeRLearner.classif.knn = function() {
  makeRLearnerClassif(
    cl = "classif.knn",
    package = "class",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "l", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "use.all", default = TRUE)
    ),
    oneclass = FALSE,
    twoclass = TRUE,
    multiclass = TRUE,
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    # knn cannot really return probs, only for the winning class (yeah well done BR)
    prob = FALSE,
    weights = FALSE
  )
}

#' @export
trainLearner.classif.knn = function(.learner, .task, .subset, .weights = NULL,  ...) {
  z = getTaskData(.task, .subset, target.extra = TRUE)
  c(list(train = z$data, cl = z$target), list(...))
}

#' @export
predictLearner.classif.knn = function(.learner, .model, .newdata, ...) {
  args = .model$learner.model
  args$test = .newdata
  do.call(knn, args)
}

