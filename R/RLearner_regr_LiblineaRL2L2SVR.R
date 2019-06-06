#' @export
makeRLearner.regr.LiblineaRL2L2SVR = function() {
  makeRLearnerRegr(
    cl = "regr.LiblineaRL2L2SVR",
    package = "LiblineaR",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = 11L, values = c(11L, 12L)),
      # FIXME default of type in LiblieaR() is 0
      makeNumericLearnerParam(id = "cost", default = 1, lower = 0),
      # FIXME: Add default value when parameter dependent defaults are implemented:
      ## if type = 11: eps default = 0.01, if type = 12: eps default = 0.1
      makeNumericLearnerParam(id = "epsilon", lower = 0),
      makeNumericLearnerParam(id = "svr_eps", lower = 0),
      makeLogicalLearnerParam(id = "bias", default = TRUE),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    # provide default to get rid of warning message during training
    par.vals = list(svr_eps = 0.1, type = 11L),
    properties = "numerics",
    name = "L2-Regularized L2-Loss Support Vector Regression",
    short.name = "liblinl2l2svr",
    note = "`type = 11` (the default) is primal and `type = 12` is dual problem. Parameter `svr_eps` has been set to `0.1` by default.",
    callees = "LiblineaR"
  )
}

#' @export
trainLearner.regr.LiblineaRL2L2SVR = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  LiblineaR::LiblineaR(data = d$data, target = d$target, ...)
}

#' @export
predictLearner.regr.LiblineaRL2L2SVR = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newx = .newdata, ...)$predictions
}
