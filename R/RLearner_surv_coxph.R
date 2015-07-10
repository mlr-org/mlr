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
    properties = c("missings", "numerics", "factors", "weights", "prob", "rcens"),
    name = "Cox Proportional Hazard Model",
    short.name = "coxph",
    note = ""
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
  if (.learner$predict.type == "prob")
    mod = attachTrainingInfo(mod, list(surv.range = range(getTaskTargets(.task)[, 1L])))
  mod
}

#' @export
predictLearner.surv.coxph = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, type = "lp", ...)
  } else if (.learner$predict.type == "prob") {
    surv.range = getTrainingInfo(.model$learner.model)$surv.range
    times = seq(from = surv.range[1L], to = surv.range[2L], length.out = 1000)
    t(summary(survival::survfit(.model$learner.model, newdata = .newdata, se.fit = FALSE, conf.int = FALSE), times = times)$surv)
  } else {
    stop("Unknown predict type")
  }
}
