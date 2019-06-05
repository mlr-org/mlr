#' @export
makeRLearner.classif.lda = function() {
  makeRLearnerClassif(
    cl = "classif.lda",
    package = "MASS",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "moment", values = c("moment", "mle", "mve", "t")),
      makeNumericLearnerParam(id = "nu", lower = 2, requires = quote(method == "t")),
      makeNumericLearnerParam(id = "tol", default = 1e-4, lower = 0),
      makeDiscreteLearnerParam(id = "predict.method", values = c("plug-in", "predictive", "debiased"),
        default = "plug-in", when = "predict"),
      makeLogicalLearnerParam(id = "CV", default = FALSE, tunable = FALSE),
      makeNumericVectorLearnerParam(id = "prior", lower = 0, upper = 1, tunable = TRUE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Linear Discriminant Analysis",
    short.name = "lda",
    note = "Learner parameter `predict.method` maps to `method` in `predict.lda`.",
    callees = c("lda", "predict.lda")
  )
}

#' @export
trainLearner.classif.lda = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  MASS::lda(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.lda = function(.learner, .model, .newdata, predict.method = "plug-in", ...) {
  p = predict(.model$learner.model, newdata = .newdata, method = predict.method, ...)
  if (.learner$predict.type == "response") {
    return(p$class)
  } else {
    return(p$posterior)
  }
}
