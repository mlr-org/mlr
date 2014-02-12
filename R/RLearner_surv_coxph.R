#' @S3method makeRLearner surv.coxph
makeRLearner.surv.coxph = function() {
  makeRLearnerSurv(
    cl = "surv.coxph",
    package = "survival",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="ties", default="efron", values=c("efron", "breslow", "exact")),
      makeLogicalLearnerParam(id="singular.ok", default=TRUE)
    ),
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

#' @S3method trainLearner surv.coxph
trainLearner.surv.coxph = function(.learner, .task, .subset, .weights,  ...) {
  if (missing(.weights)) {
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

#' @S3method predictLearner surv.coxph
predictLearner.surv.coxph = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata=.newdata, type="risk", ...)
  else
    stop("Unknown predict type")
}
