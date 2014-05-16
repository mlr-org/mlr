#' @export
makeRLearner.surv.glmboost = function() {
  makeRLearnerSurv(
    cl = "surv.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="family", default=CoxPH(), values=list(CoxPH=CoxPH(), Weibull=Weibull(), Loglog=Loglog(), Lognormal=Lognormal())),
      # makeDiscreteLearnerParam(id="family", values=c("CoxPH", "Weibull", "Loglog", "Lognormal"), default="CoxPH"),
      makeIntegerLearnerParam(id="mstop", default=100L, lower=1L),
      makeNumericLearnerParam(id="nu", default=0.1, lower=0, upper=1),
      makeLogicalLearnerParam(id="center", default=FALSE)
    ),
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

#' @export
trainLearner.surv.glmboost = function(.learner, .task, .subset, .weights = NULL, mstop, nu, ...) {
  f = getTaskFormula(.task, env=as.environment("package:survival"))
  ctrl = learnerArgsToControl(boost_control, mstop, nu)
  if (is.null(.weights)) {
    glmboost(f, data=getTaskData(.task, .subset), control=ctrl, family=CoxPH())
  } else  {
    glmboost(f, data=getTaskData(.task, .subset), control=ctrl, weights=.weights, ...)
  }
}

#' @export
predictLearner.surv.glmboost = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata = .newdata, type = "link")
  else
    stop("Unknown predict type")
}
