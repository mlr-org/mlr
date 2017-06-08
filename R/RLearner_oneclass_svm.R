#' @export
makeRLearner.oneclass.svm = function() {
  makeRLearnerOneClass(
    cl = "oneclass.svm",
    package = "e1071",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "nu", default = 0.5),
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
    properties =  c("oneclass", "numerics", "factors", "prob"),
    note = "'type' is set to 'one-classification'",
    name = "one-class SVM (libsvm)",
    short.name = "svm",
    callees = "svm"
  )
}

#' @export
trainLearner.oneclass.svm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  z = getTaskData(.task, .subset, target.extra = TRUE)
  e1071::svm(z$data, y = NULL, type = "one-classification", ...)
}

#' @export
predictLearner.oneclass.svm = function(.learner, .model, .newdata, ...) {
  td = getTaskDesc(.model)
  label = c(td$positive, td$negative)
  if (.learner$predict.type == "response") {
    p = predict(.model$learner.model, newdata = .newdata, ...)
    p = factor(p, levels = c("TRUE", "FALSE"), labels = label)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, decision.values = TRUE, ...)
    p = attr(p, "decision.values")
    p = convertingScoresToProbability(p, parainit = c(1, 0))$probability
    p = cbind(p, 1-p)
    colnames(p) = label
  }
  return(p)
}


