#' @export
makeRLearner.regr.svm = function() {
  makeRLearnerRegr(
    cl = "regr.svm",
    package = "e1071",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = "eps-regression", values = c("eps-regression", "nu-regression")),
      makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = expression(type=="C-regrication")),
      makeNumericLearnerParam(id = "epsilon", lower = 0, requires = expression(type == "eps-regression")),
      makeNumericLearnerParam(id = "nu", default = 0.5, requires = expression(type=="nu-regression")),
      makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = expression(kernel=="polynomial")),
      makeNumericLearnerParam(id = "coef0", default = 0, requires = expression(kernel=="polynomial" || kernel=="sigmoid")),
      makeNumericLearnerParam(id = "gamma", lower = 0, requires = expression(kernel!="linear")),
      makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeNumericLearnerParam(id = "cachesize", default = 40L)

    ),
    properties = c("numerics", "factors"),
    name = "Support Vector Machines (libsvm)",
    short.name = "svm",
    note = ""
  )
}

#' @export
trainLearner.regr.svm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  e1071::svm(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.svm = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}
