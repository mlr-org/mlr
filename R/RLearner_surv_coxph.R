#' @export
makeRLearner.surv.coxph = function() {
  makeRLearnerSurv(
    cl = "surv.coxph",
    package = "survival",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "ties", default = "efron", values = c("efron", "breslow", "exact")),
      makeLogicalLearnerParam(id = "singular.ok", default = TRUE),
      makeNumericLearnerParam(id = "eps", default = 1e-09, lower = 0),
      makeNumericLearnerParam(id = "toler.chol", default = .Machine$double.eps^0.75, lower = 0),
      makeIntegerLearnerParam(id = "iter.max", default = 20L, lower = 1L),
      makeNumericLearnerParam(id = "toler.inf", default = sqrt(.Machine$double.eps^0.75), lower = 0),
      makeIntegerLearnerParam(id = "outer.max", default = 10L, lower = 1L),
      makeLogicalLearnerParam(id = "model", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "x", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "y", default = TRUE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "weights", "rcens"),
    name = "Cox Proportional Hazard Model",
    short.name = "coxph"
  )
}

#' @export
trainLearner.surv.coxph = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  data = getTaskData(.task, subset = .subset)
  if (is.null(.weights)) {
    mod = survival::coxph(formula = f, data = data, ...)
  } else  {
    mod = survival::coxph(formula = f, data = data, weights = .weights, ...)
  }
  attachTrainingInfo(mod, list(surv.train = getTaskTargets(.task, .subset, recode.target = "rcens")))
}

#' @export
predictLearner.surv.coxph = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, type = "lp", ...)
  }
}
