#' @export
makeRLearner.classif.LiblineaRL1LogReg = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRL1LogReg",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      makeNumericLearnerParam(id = "epsilon", default = 0.01, lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "class.weights", "prob"),
    class.weights.param = "wi",
    name = "L1-Regularized Logistic Regression",
    short.name = "liblinl1logreg",
    callees = "LiblineaR"
  )
}

#' @export
trainLearner.classif.LiblineaRL1LogReg = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, type = 6L, ...)
}

#' @export
predictLearner.classif.LiblineaRL1LogReg = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
  } else {
    predict(.model$learner.model, newx = .newdata, proba = TRUE, ...)$probabilities
  }
}
