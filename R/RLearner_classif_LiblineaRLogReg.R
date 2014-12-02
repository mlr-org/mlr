#' @export
makeRLearner.classif.LiblineaRLogReg = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRLogReg",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", values = c(0, 6, 7)),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      #default epsilon is dependet on type
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE)
    ),
  properties = c("twoclass", "numerics", "prob"),
  name = "Regularized Logistic Regression",
  short.name = "reglreg",
  note = "This model subsumes type 0,6,7."
  )
}

#' @export
trainLearner.classif.LiblineaRLogReg = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, labels = d$target, ...)
}

#' @export
predictLearner.classif.LiblineaRLogReg = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
  else
    predict(.model$learner.model, newx = .newdata, proba = TRUE, ...)$probabilities
}
