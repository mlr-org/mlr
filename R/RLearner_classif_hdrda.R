#' @export
makeRLearner.classif.hdrda = function() {
  makeRLearnerClassif(
    cl = "classif.hdrda",
    package = "sparsediscrim",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0),
      makeDiscreteLearnerParam(id = "shrinkage_type", default = "ridge", values = c("ridge", "convex")),
      makeNumericVectorLearnerParam(id = "prior"),
      makeNumericLearnerParam(id = "tol", default = 1e-06),
      makeLogicalLearnerParam(id = "projected", default = FALSE)
    ),
    properties = c("numerics", "twoclass", "prob"),
    name = "High-Dimensional Regularized Discriminant Analysis",
    short.name = "hdrda",
    note = ""
  )
}

#' @export
trainLearner.classif.hdrda = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  sparsediscrim::hdrda(x = as.matrix(d$data), y = d$target, ...)
}

#' @export
predictLearner.classif.hdrda = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, as.matrix(.newdata), ...)
  if (.learner$predict.type == "response") {
    return(p$class)
  } else {
    return(p$posterior)
  }
}
