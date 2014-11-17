#' @export
makeRLearner.surv.glmboost = function() {
  makeRLearnerSurv(
    cl = "surv.glmboost",
    package = c("survival", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", values = c("CoxPH", "Weibull", "Loglog", "Lognormal"), default = "CoxPH"),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "center", default = FALSE),
      makeDiscreteLearnerParam(id = "m", default = "mstop", values = c("mstop", "cv"))
    ),
    par.vals = list(
      family = "CoxPH",
      m = "mstop"
    ),
    properties = c("numerics", "factors", "ordered", "weights", "rcens"),
    name = "Gradient Boosting with Componentwise Linear Models",
    short.name = "glmboost",
    note = paste(
      "`family` has been set to `CoxPH()` by default.",
      "Maximum number of boosting iterations is set via 'mstop', the actual number used for prediction is controlled by 'm'."
    )
  )
}

#' @export
trainLearner.surv.glmboost = function(.learner, .task, .subset, .weights = NULL, family, mstop, nu, m, ...) {
  family = do.call(get(family, mode = "function", envir = as.environment("package:mboost")), list())
  f = getTaskFormula(.task, env = as.environment("package:survival"))
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu)
  if (is.null(.weights)) {
    model = mboost::glmboost(f, data = getTaskData(.task, .subset), control = ctrl, family = family, ...)
  } else  {
    model = mboost::glmboost(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, family = family, ...)
  }

  if (m == "cv") {
    mstop(model) = mstop(cvrisk(model, papply = lapply))
  }
  model
}

#' @export
predictLearner.surv.glmboost = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata = .newdata, type = "link")
  else
    stop("Unknown predict type")
}
