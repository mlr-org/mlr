#' @export
makeRLearner.classif.LiblineaRL2L1SVC = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRL2L1SVC",
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
    name = "L2-Regularized L1-Loss Support Vector Classification",
    short.name = "liblinl2l1svc",
    callees = "LiblineaR"
  )
}

#' @export
trainLearner.classif.LiblineaRL2L1SVC = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, type = 3L, ...)
}

#' @export
predictLearner.classif.LiblineaRL2L1SVC = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
}
