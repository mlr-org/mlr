#' @export
makeRLearner.classif.qda = function() {
  makeRLearnerClassif(
    cl = "classif.qda",
    package = "MASS",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "moment", values = c("moment", "mle", "mve", "t")),
      makeNumericLearnerParam(id = "nu", default = 5, lower = 2, requires = quote(method == "t")),
      makeDiscreteLearnerParam(id = "predict.method", values = c("plug-in", "predictive", "debiased"),
        default = "plug-in", when = "predict")
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Quadratic Discriminant Analysis",
    short.name = "qda",
    note = "Learner parameter `predict.method` maps to `method` in `predict.qda`.",
    callees = c("qda", "predict.qda")
  )
}

#' @export
trainLearner.classif.qda = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  MASS::qda(f, data = getTaskData(.task, .subset, recode.target = "drop.levels"), ...)
}

#' @export
predictLearner.classif.qda = function(.learner, .model, .newdata, predict.method = "plug-in", ...) {
  p = predict(.model$learner.model, newdata = .newdata, method = predict.method, ...)
  if (.learner$predict.type == "response") {
    return(p$class)
  } else {
    return(p$posterior)
  }
}
