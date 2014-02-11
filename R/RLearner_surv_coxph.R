#' @S3method makeRLearner surv.coxph
makeRLearner.surv.coxph = function() {
  makeRLearnerSurv(
    cl = "surv.coxph",
    package = "survival",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="ties", default="efron", values=c("efron", "breslow", "exact")),
      makeLogicalLearnerParam(id="singular.ok", default=TRUE)
    ),
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

#' @S3method trainLearner surv.coxph
trainLearner.surv.coxph = function(.learner, .task, .subset, .weights,  ...) {
  # FIXME: setting the environment to NULL is a horrible idea.
  # This completely breaks the function lookup
  if (missing(.weights)) {
    coxph(getTaskFormula(.task), getTaskData(.task, subset=.subset))
  } else  {
    #...
  }
}

#' @S3method predictLearner surv.coxph
predictLearner.surv.coxph = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata=.newdata, ...)
  else
    stop("Unknown predict type")
}
