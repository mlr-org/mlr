#' @export
makeRLearner.regr.glmboost = function() {
  makeRLearnerRegr(
    cl = "regr.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = mboost::Gaussian(),
        values = list(Gaussian = mboost::Gaussian(), Laplace = mboost::Laplace(),
          Huber = mboost::Huber(), Poisson = mboost::Poisson(), GammaReg = mboost::GammaReg(nuirange = c(0,100)),
          NBinomial = mboost::NBinomial(), Hurdle = mboost::Hurdle())),
      #makeUntypedLearnerParam(id = "custom.family.defintion", requires = quote(family = "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      makeLogicalLearnerParam(id = "center", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
      ),
    par.vals = list(),
    # FIXME Parameter m not found in help of glmboost() or mboost_fit() nor in mstop(), par.vals and LernerParam default are same
    properties = c("numerics", "factors", "weights"),
    name = "Boosting for GLMs",
    short.name = "glmboost"
  )
}

#' @export
trainLearner.regr.glmboost = function(.learner, .task, .subset, .weights = NULL, mstop, nu, m, risk, trace, stopintern, family, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, trace, stopintern)
  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task)
  # if (!is.null(custom.family)) {
  #  family = custom.family
  # } else {
  #   family = family
  # }
  if (is.null(.weights)) {
    model = mboost::glmboost(f, data = d, control = ctrl, ...)
  } else {
    model = mboost::glmboost(f, data = d, control = ctrl, weights = .weights, ...)
  }
  model
}

#' @export
predictLearner.regr.glmboost = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, ...)
  return(as.vector(p))
}
