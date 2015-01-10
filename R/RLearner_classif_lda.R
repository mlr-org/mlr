#' @export
makeRLearner.classif.lda = function() {
  makeRLearnerClassif(
    cl = "classif.lda",
    package = "MASS",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "moment", values = c("moment", "mle", "mve", "t")),
      makeNumericLearnerParam(id = "nu", lower = 2, requires = expression(method=="t")),
      makeNumericLearnerParam(id = "tol", default = 1e-4, lower = 0),
      makeDiscreteLearnerParam(id = "predict.method", values = c("plug-in", "predictive", "debiased"),
        default = "plug-in", when = "predict")
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Linear Discriminant Analysis",
    short.name = "lda",
    note = "Learner param 'predict.method' maps to 'method' in predict.lda."
  )
}

#' @export
trainLearner.classif.lda = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  MASS::lda(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.lda = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, ...)
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}

