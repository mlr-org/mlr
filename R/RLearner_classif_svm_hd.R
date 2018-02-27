#' @export
makeRLearner.classif.svm.hd = function() {
  makeRLearnerClassif(
    cl = "classif.svm.hd",
    package = "e1071",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
      makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = quote(type == "C-classification")),
      makeNumericLearnerParam(id = "nu", default = 0.5, requires = quote(type == "nu-classification")),
      makeNumericVectorLearnerParam("class.weights", len = NA_integer_, lower = 0),
      makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = quote(kernel == "polynomial")),
      makeNumericLearnerParam(id = "coef0", default = 0, requires = quote(kernel == "polynomial" || kernel == "sigmoid")),
      makeNumericLearnerParam(id = "gamma", lower = 0, requires = quote(kernel != "linear")),
      makeNumericLearnerParam(id = "cachesize", default = 40L),
      makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "fitted", default = TRUE, tunable = FALSE),
      makeLogicalVectorLearnerParam(id = "scale", default = TRUE, tunable = TRUE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "class.weights",
    name = "Support Vector Machines (libsvm)",
    short.name = "svm.hd",
    callees = "svm.hd"
  )
}

#' @export
trainLearner.classif.svm.hd = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  d = getTaskData(task = .task, subset = .subset, target.extra = TRUE)
  e1071::svm(x = d$data, y = d$target, probability = .learner$predict.type == "prob", ...)
}

#' @export
predictLearner.classif.svm.hd = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, ...)
  } else {
    attr(predict(.model$learner.model, newdata = .newdata, probability = TRUE, ...), "probabilities")
  }
}
