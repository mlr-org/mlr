#' @export
makeRLearner.classif.svm = function() {
  makeRLearnerClassif(
    cl = "classif.svm",
    package = "e1071",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
      makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = expression(type=="C-classification")),
      makeNumericLearnerParam(id = "nu", default = 0.5, requires = expression(type=="nu-classification")),
      makeNumericVectorLearnerParam("class.weights", len = NA_integer_, lower = 0),
      makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = expression(kernel=="polynomial")),
      makeNumericLearnerParam(id = "coef0", default = 0, requires = expression(kernel=="polynomial" || kernel=="sigmoid")),
      makeNumericLearnerParam(id = "gamma", lower = 0, requires = expression(kernel!="linear")),
      makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeNumericLearnerParam(id = "cachesize", default = 40L)

    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "classif.svm",
    short.name = "svm",
    note = ""
  )
}

#' @export
trainLearner.classif.svm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  svm(f, data = getTaskData(.task, .subset), probability = .learner$predict.type == "prob", ...)
}

#' @export
predictLearner.classif.svm = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, ...)
  } else {
    attr(predict(.model$learner.model, newdata = .newdata, probability = TRUE, ...), "probabilities")
  }
}
