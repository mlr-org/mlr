#' @export
makeRLearner.oneclass.svm = function() {
  makeRLearnerOneClass(
    cl = "oneclass.svm",
    package = "e1071",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = "one-classification", values = "one-classification"),
      makeNumericLearnerParam(id = "nu", default = 0.5, requires = quote(type == "nu-classification" || type == "one-classification" || type == "nu-regression")),
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
    par.vals = list(type = "one-classification"),
    properties =  c("oneclass", "numerics", "factors", "weights"),
    name = "one-class Support Vector Machines (libsvm)",
    short.name = "one-class svm",
    callees = "svm"
  )
}

#' @export
trainLearner.oneclass.svm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, .subset)[, x]
    e1071::svm(d, y = NULL, ...)
}

#' @export
predictLearner.oneclass.svm = function(.learner, .model, .newdata, ...) {
  # svm currently can't predict probabilities only response
   p = predict(.model$learner.model, newdata = .newdata, ...)
   if (.learner$predict.type == "response") {
     p = as.factor(p)
     levels(p) = union(levels(p), .model$task.desc$class.levels)
   }
  return(p)
}


