#' @export
makeRLearner.surv.randomForestSRC = function() {
  makeRLearnerSurv(
    cl = "surv.randomForestSRC",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="ntree", default=1000L, lower=1L),
      makeDiscreteLearnerParam(id="bootstrap", values=c("by.root", "by.node", "none"), default="by.root"),
      makeIntegerLearnerParam(id="mtry", lower=1L),
      makeIntegerLearnerParam(id="nodesize", lower=1L, default=3L),
      makeDiscreteLearnerParam(id="splitrule", values=c("logrank", "logrankscore"), default="logrank"),
      makeDiscreteLearnerParam(id="na.action", values=c("na.omit", "na.impute"), default="na.omit")
    ),
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

#' @export
trainLearner.surv.randomForestSRC = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task, env=as.environment("package:survival"))
  rfsrc(getTaskFormula(.task), data = getTaskData(.task, .subset), importance = "none", proximity = FALSE, forest = TRUE, ...)
}

#' @export
predictLearner.surv.randomForestSRC = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata=.newdata, importance = "none", na.action = "na.impute", ...)$predicted
  } else {
    stop("Unknown predict type")
  }
}
