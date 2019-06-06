#' @export
makeRLearner.classif.LiblineaRL2SVC = function() {
  makeRLearnerClassif(
    cl = "classif.LiblineaRL2SVC",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = 2L, values = c(1L, 2L)),
      # FIXME default in LiblieaR for type is 0.
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      ## FIXME: Add default value when parameter dependent defaults are implemented:
      ## if type = 1: eps default = 0.1, if type = 2: eps default = 0.01
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeNumericVectorLearnerParam(id = "wi", len = NA_integer_),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(type = 2L),
    properties = c("twoclass", "multiclass", "numerics", "class.weights"),
    class.weights.param = "wi",
    name = "L2-Regularized L2-Loss Support Vector Classification",
    short.name = "liblinl2svc",
    note = "`type = 2` (the default) is primal and `type = 1` is dual problem.",
    callees = "LiblineaR"
  )
}

#' @export
trainLearner.classif.LiblineaRL2SVC = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, ...)
}

#' @export
predictLearner.classif.LiblineaRL2SVC = function(.learner, .model, .newdata, ...) {
  as.factor(predict(.model$learner.model, newx = .newdata, ...)$predictions)
}
