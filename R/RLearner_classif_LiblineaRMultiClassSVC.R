#' @export
makeRLearner.classif.LiblineaRMultiClassSVC = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRMultiClassSVC",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      makeNumericLearnerParam(id = "epsilon", default = 0.1, lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "class.weights"),
    class.weights.param = "wi",
    name = "Support Vector Classification by Crammer and Singer",
    short.name = "liblinmulticlasssvc",
    callees = "LiblineaR"
  )
}

#' @export
trainLearner.classif.LiblineaRMultiClassSVC = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, type = 4L, ...)
}

#' @export
predictLearner.classif.LiblineaRMultiClassSVC = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
}
