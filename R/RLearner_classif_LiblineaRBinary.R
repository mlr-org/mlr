#' @export
makeRLearner.classif.LiblineaRBinary = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRBinary",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", values = c(1, 2, 3, 5)),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      # better default epsilon is dependent on type ( = NULL, see docs), but we cannot store this
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "numerics"),
    name = "Regularized Binary Linear Predictive Models Estimation",
    short.name = "liblinearbinary",
    note = "This model subsumes the types 1,2,3,5."
  )
}

#' @export
trainLearner.classif.LiblineaRBinary = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, ...)
}

#' @export
predictLearner.classif.LiblineaRBinary = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
}
