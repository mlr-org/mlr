#' @export
makeRLearner.classif.LiblineaRMultiClass = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRMultiClass",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", values = 4),
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      makeNumericLearnerParam(id = "epsilon", default = 0.1, lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(type = 4),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Multi-class Support Vector Classification by Crammer and Singer",
    short.name = "mcsvc",
    note = "This model is type 4."
  )
}

#' @export
trainLearner.classif.LiblineaRMultiClass = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, ...)
}

#' @export
predictLearner.classif.LiblineaRMultiClass = function(.learner, .model, .newdata, ...) {
  p = as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
}
