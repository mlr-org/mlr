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
    properties = c("numerics", "factors", "weights", "prob"),
    name = "Cox Proportional Hazard Model",
    short.name = "coxph",
    callees = c("coxph", "coxph.control")
  )
}

#' @export
trainLearner.surv.coxph = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  data = getTaskData(.task, subset = .subset)
  if (.learner$predict.type == "response") {
    survival::coxph(formula = f, data = data, weights = .weights, ...)
  } else  {
    model = survival::coxph(formula = f, data = data, weights = .weights, ...)
    model = attachTrainingTime(model, .task, data)
    model
  }
}

#' @export
predictLearner.surv.coxph = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, type = "lp", ...)
  } else {
    preds = predict(.model$learner.model, newdata = .newdata, type = "lp", ...)
    train.times = sort(unique(c(0, .model$learner.model$times)))
    probs = pec::predictSurvProb(.model$learner.model, newdata = .newdata, times = train.times)
    list(preds = preds, probs = probs, train.times = train.times)
  }
}
