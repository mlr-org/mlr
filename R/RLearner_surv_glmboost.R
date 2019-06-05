#' @export
makeRLearner.surv.glmboost = function() {
  makeRLearnerSurv(
    cl = "surv.glmboost",
    package = c("!survival", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "CoxPH", values = c("CoxPH", "Weibull", "Loglog", "Lognormal", "Gehan", "custom.family")),
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0, 100), requires = quote(family %in% c("Weibull", "Loglog", "Lognormal"))),
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      # 'risk' and 'stopintern' will be kept for completeness sake
      makeLogicalLearnerParam(id = "center", default = FALSE),
      makeLogicalLearnerParam(id = "use.formula", default = TRUE, when = "both"),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(
      family = "CoxPH",
      use.formula = TRUE
    ),
    properties = c("numerics", "factors", "ordered", "weights"),
    name = "Gradient Boosting with Componentwise Linear Models",
    short.name = "glmboost",
    note = "`family` has been set to `CoxPH()` by default.",
    callees = c("glmboost", "mboost_fit", "boost_control", "CoxPH", "Weibull", "Loglog", "Lognormal", "Gehan")
  )
}

#' @export
trainLearner.surv.glmboost = function(.learner, .task, .subset, .weights = NULL, nuirange = c(0, 100), family, custom.family.definition, mstop, nu, risk, stopintern, trace, use.formula, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, trace, stopintern)
  family = switch(family,
    CoxPH = mboost::CoxPH(),
    Weibull = mboost::Weibull(nuirange = nuirange),
    Loglog = mboost::Loglog(nuirange = nuirange),
    Lognormal = mboost::Lognormal(nuirange = nuirange),
    Gehan = mboost::Gehan(),
    custom.family = custom.family.definition
  )
  if (use.formula) {
    f = getTaskFormula(.task)
    model = if (is.null(.weights)) {
      mboost::glmboost(f, data = getTaskData(.task, subset = .subset, recode.target = "surv"), control = ctrl, family = family, ...)
    } else {
      mboost::glmboost(f, data = getTaskData(.task, subset = .subset, recode.target = "surv"), control = ctrl, weights = .weights, family = family, ...)
    }
  } else {
    data = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "surv")
    info = getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
    data$data = as.matrix(fixDataForLearner(data$data, info))
    model = if (is.null(.weights)) {
      mboost::glmboost(x = data$data, y = data$target, control = ctrl, family = family, ...)
    } else {
      mboost::glmboost(x = data$data, y = data$target, control = ctrl, weights = .weights, family = family, ...)
    }
    model = attachTrainingInfo(model, info)
  }
  model
}

#' @export
predictLearner.surv.glmboost = function(.learner, .model, .newdata, use.formula, ...) {
  if (!use.formula) {
    info = getTrainingInfo(.model)
    .newdata = as.matrix(fixDataForLearner(.newdata, info))
  }
  predict(.model$learner.model, newdata = .newdata, type = "link")
}
