#' @export
makeRLearner.surv.gamboost = function() {
  makeRLearnerSurv(
    cl = "surv.gamboost",
    package = c("!survival", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "baselearner", values = c("bbs", "bols", "btree")),
      makeIntegerLearnerParam(id = "dfbase", default = 4L),
      makeNumericLearnerParam(id = "offset"),
      makeDiscreteLearnerParam(id = "family", default = "CoxPH", values = c("CoxPH", "Weibull", "Loglog", "Lognormal", "Gehan", "custom.family")),
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0, 100), requires = quote(family %in% c("Weibull", "Loglog", "Lognormal"))),
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      # 'risk' and 'stopintern' will be kept for completeness sake
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(
      family = "CoxPH"
    ),
    properties = c("numerics", "factors", "ordered", "weights"),
    name = "Gradient boosting with smooth components",
    short.name = "gamboost",
    note = "`family` has been set to `CoxPH()` by default.",
    callees = c("gamboost", "mboost_fit", "boost_control", "CoxPH", "Weibull", "Loglog", "Lognormal", "Gehan")
  )
}

#' @export
trainLearner.surv.gamboost = function(.learner, .task, .subset, .weights = NULL, nuirange = c(0, 100), family, custom.family.definition, mstop, nu, risk, stopintern, trace, ...) {
  requirePackages("mboost", why = "argument 'baselearner' requires package", suppress.warnings = TRUE)
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, trace, stopintern)
  family = switch(family,
    CoxPH = mboost::CoxPH(),
    Weibull = mboost::Weibull(nuirange = nuirange),
    Loglog = mboost::Loglog(nuirange = nuirange),
    Lognormal = mboost::Lognormal(nuirange = nuirange),
    Gehan = mboost::Gehan(),
    custom.family = custom.family.definition
  )

  f = getTaskFormula(.task)
  data = getTaskData(.task, subset = .subset, recode.target = "surv")
  if (is.null(.weights)) {
    model = mboost::gamboost(f, data = data, control = ctrl, family = family, ...)
  } else {
    model = mboost::gamboost(f, data = getTaskData(.task, subset = .subset, recode.target = "surv"), control = ctrl, weights = .weights, family = family, ...)
  }
}

#' @export
predictLearner.surv.gamboost = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, type = "link")
}
