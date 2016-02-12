#' @export
makeRLearner.regr.glmboost = function() {
  makeRLearnerRegr(
    cl = "regr.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = mboost::Gaussian(), values = list(Gaussian = mboost::Gaussian(), Laplace = mboost::Laplace())),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      makeLogicalLearnerParam(id = "center", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "m", default = "mstop", values = c("mstop", "cv", "aic"))
      ),
    par.vals = list(m = "mstop"),
    properties = c("numerics", "factors", "weights"),
    name = "Boosting for GLMs",
    short.name = "glmboost",
    note = "Maximum number of boosting iterations is set via `mstop`, the actual number used is controlled by `m`."
  )
}
#' @export
trainLearner.regr.glmboost = function(.learner, .task, .subset, .weights = NULL, mstop, nu, m, risk, trace, stopintern, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, trace, stopintern)
  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task)
  if (is.null(.weights)) {
    model = mboost::glmboost(f, data = d, control = ctrl, ...)
  } else {
    model = mboost::glmboost(f, data = d, control = ctrl, weights = .weights, ...)
  }
  if (m == "cv") {
    mboost::mstop(model) = mboost::mstop(mboost::cvrisk(model, papply = lapply))
  } else if (m == "aic") {
    mboost::mstop(model) = mboost::mstop(AIC(model, method = "classical"))
  }
  model
}
#' @export
predictLearner.regr.glmboost = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, ...)
  return(as.vector(p))
}
