#' @export
makeRLearner.regr.svm = function() {
  makeRLearnerRegr(
    cl = "regr.svm",
    package = "e1071",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = "eps-regression", values = c("eps-regression", "nu-regression")),
      makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = quote(kernel == "polynomial")),
      makeNumericLearnerParam(id = "gamma", lower = 0, requires = quote(kernel != "linear")),
      makeNumericLearnerParam(id = "coef0", default = 0, requires = quote(kernel == "polynomial" || kernel == "sigmoid")),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0, requires = quote(type == "C-regrication")),
      makeNumericLearnerParam(id = "nu", default = 0.5, requires = quote(type == "nu-regression")),
      makeNumericLearnerParam(id = "cachesize", default = 40L),
      makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
      makeNumericLearnerParam(id = "epsilon", lower = 0, requires = quote(type == "eps-regression")),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "fitted", default = TRUE, tunable = FALSE),
      makeLogicalVectorLearnerParam(id = "scale", default = TRUE, tunable = TRUE)
    ),
    properties = c("numerics", "factors"),
    name = "Support Vector Machines (libsvm)",
    short.name = "svm",
    callees = "svm"
  )
}

#' @export
trainLearner.regr.svm = function(.learner, .task, .subset, .weights = NULL, ...) {
  if (sum(getTaskDesc(.task)$n.feat[c("factors", "ordered")]) > 0) {
    f = getTaskFormula(.task)
    e1071::svm(f, data = getTaskData(.task, .subset), ...)
  } else {
    d = getTaskData(.task, .subset, target.extra = TRUE)
    e1071::svm(d$data, d$target, ...)
  }
}

#' @export
predictLearner.regr.svm = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}
