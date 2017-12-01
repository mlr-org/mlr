#' @export
makeRLearner.regr.elmNN = function() {
  makeRLearnerRegr(
    cl = "regr.elmNN",
    package = "elmNN",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "nhid", default = 1L, lower = 1L),
      makeDiscreteLearnerParam(id = "actfun", default = "sig", values = c("sig", "sin", "radbas",
        "hardlim", "hardlims", "satlins", "tansig", "tribas", "poslin", "purelin"))
      # FIXME default of nhid and actfun not in the help, if the defaults are correct, par.vals is redundant
    ),
    par.vals = list(nhid = 1L, actfun = "sig"),
    properties = "numerics",
    name = "Extreme Learning Machine for Single Hidden Layer Feedforward Neural Networks",
    short.name = "elmNN",
    note = '`nhid` has been set to `1` and `actfun` has been set to `"sig"` by default.',
    callees = "elmtrain.default"
  )
}

#' @export
trainLearner.regr.elmNN = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  elmNN::elmtrain.default(x = as.matrix(d$data), y = d$target, ...)
}

#' @export
predictLearner.regr.elmNN = function(.learner, .model, .newdata, ...) {
  elmNN::predict.elmNN(.model$learner.model, newdata = as.matrix(.newdata), ...)[, 1L]
}
