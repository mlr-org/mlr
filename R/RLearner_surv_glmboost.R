#' @export
makeRLearner.surv.glmboost = function() {
  makeRLearnerSurv(
    cl = "surv.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", values = c("CoxPH", "Weibull", "Loglog", "Lognormal"), default = "CoxPH"),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "center", default = FALSE)
    ),
    par.vals = list(
      family = "CoxPH"
    ),
    properties = c("numerics", "factors", "weights", "rcens"),
    name = "Gradient Boosting with Componentwise Linear Models",
    short.name = "glmboost",
    note = ""
  )
}

#' @export
trainLearner.surv.glmboost = function(.learner, .task, .subset, .weights = NULL, family, mstop, nu, ...) {
  family = "CoxPH"
  family = do.call(get(family, mode = "function", envir = as.environment("package:mboost")), list())
  f = getTaskFormula(.task, env = as.environment("package:survival"))
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu)
  if (is.null(.weights)) {
    mboost::glmboost(f, data = getTaskData(.task, .subset), control = ctrl, family = family, ...)
  } else  {
    mboost::glmboost(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, family = family, ...)
  }
}

#' @export
predictLearner.surv.glmboost = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata = .newdata, type = "link")
  else
    stop("Unknown predict type")
}
