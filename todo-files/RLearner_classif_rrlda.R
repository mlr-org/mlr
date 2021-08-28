#' @export
makeRLearner.classif.rrlda = function() {
  makeRLearnerClassif(
    cl = "classif.rrlda",
    package = "!rrlda",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "prior", len = NA_integer_),
      makeNumericLearnerParam(id = "lambda", default = 0.5, lower = 0),
      makeNumericLearnerParam(id = "hp", default = 0.75, lower = 0),
      makeIntegerLearnerParam(id = "nssamples", default = 30L, lower = 1L),
      makeIntegerLearnerParam(id = "maxit", default = 50L, lower = 1L),
      makeDiscreteLearnerParam(id = "penalty", default = "L2", values = c("L1", "L2"))
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Robust Regularized Linear Discriminant Analysis",
    short.name = "rrlda",
    callees = "rrlda"
  )
}

#' @export
trainLearner.classif.rrlda = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "drop.levels")
  rrlda::rrlda(x = d$data, grouping = d$target, ...)
}

#' @export
predictLearner.classif.rrlda = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, x = .newdata, ...)$class)
}
