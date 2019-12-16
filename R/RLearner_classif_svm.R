#' @export
makeRLearner.classif.svm = function() {
  makeRLearnerClassif(
    cl = "classif.svm",
    package = "e1071",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0, requires = quote(type == "C-classification")),
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
    short.name = "svm",
    callees = "svm"
  )
}

#' @export
trainLearner.classif.svm = function(.learner, .task, .subset, .weights = NULL, ...) {
  if (sum(getTaskDesc(.task)$n.feat[c("factors", "ordered")]) > 0) {
    # use formula interface if factors are present
    f = getTaskFormula(.task)
    e1071::svm(f, data = getTaskData(.task, .subset), probability = .learner$predict.type == "prob", ...)
  } else {
    # use the "data.frame" approach if no factors are present to prevent issues like https://github.com/mlr-org/mlr/issues/1738
    d = getTaskData(.task, .subset, target.extra = TRUE)
    e1071::svm(d$data, d$target, probability = .learner$predict.type == "prob", ...)
  }
}

#' @export
predictLearner.classif.svm = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, ...)
  } else {
    attr(predict(.model$learner.model, newdata = .newdata, probability = TRUE, ...), "probabilities")
  }
}
