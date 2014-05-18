#' @export
makeRLearner.surv.coxph = function() {
  makeRLearnerSurv(
    cl = "surv.coxph",
    package = "survival",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="ties", default="efron", values=c("efron", "breslow", "exact")),
      makeLogicalLearnerParam(id="singular.ok", default=TRUE),
      makeNumericLearnerParam(id="eps", default=1e-09, lower=0),
      makeNumericLearnerParam(id="toler.chol", default=.Machine$double.eps^0.75, lower=0),
      makeIntegerLearnerParam(id="iter.max", default=20L, lower=1L),
      makeNumericLearnerParam(id="toler.inf", default=sqrt(.Machine$double.eps^0.75), lower=0),
      makeIntegerLearnerParam(id="outer.max", default=10L, lower=1L)
    ),
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

#' @export
trainLearner.surv.coxph = function(.learner, .task, .subset, .weights = NULL,  ...) {
  if (is.null(.weights)) {
    coxph(
      formula = getTaskFormula(.task, env=as.environment("package:survival")),
      data = getTaskData(.task, subset=.subset),
      ...)
  } else  {
    coxph(
      formula = getTaskFormula(.task, env=.GlobalEnv),
      data = getTaskData(.task, subset=.subset),
      weights = .weights,
      ...)
  }
}

#' @export
predictLearner.surv.coxph = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata=.newdata, type="lp", ...)
  else
    stop("Unknown predict type")
}
