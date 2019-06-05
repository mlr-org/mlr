#' @export
makeRLearner.regr.LiblineaRL2L1SVR = function() {
  makeRLearnerRegr(
    cl = "regr.LiblineaRL2L1SVR",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      makeNumericLearnerParam(id = "epsilon", default = 0.1, lower = 0),
      makeNumericLearnerParam(id = "svr_eps", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    # provide default to get rid of warning message during training
    par.vals = list(svr_eps = 0.1),
    properties = "numerics",
    name = "L2-Regularized L1-Loss Support Vector Regression",
    short.name = "liblinl2l1svr",
    note = "Parameter `svr_eps` has been set to `0.1` by default.",
    callees = "LiblineaR"
  )
}

#' @export
trainLearner.regr.LiblineaRL2L1SVR = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, type = 13L, ...)
}

#' @export
predictLearner.regr.LiblineaRL2L1SVR = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newx = .newdata, ...)$predictions
}
