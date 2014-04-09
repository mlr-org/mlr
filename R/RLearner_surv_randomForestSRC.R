#' @S3method makeRLearner surv.randomForestSRC
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

#' @S3method trainLearner surv.randomForestSRC
trainLearner.surv.randomForestSRC = function(.learner, .task, .subset, .weights,  ...) {
  #FIXME: unnecessary data duplication
  data = getTaskData(.task, subset=.subset, target.extra=FALSE)
  rfsrc(getTaskFormula(.task), data = data, importance="none", proximity=FALSE, ...)
}

#' @S3method predictLearner surv.randomForestSRC
predictLearner.surv.randomForestSRC = function(.learner, .model, .newdata, ...) {
  s = .model$learner.model$lambda.min
  if(.learner$predict.type == "response")
    predict(.model$learner.model, newdata=.newdata, importance = "none", ...)
  else
    stop("Unknown predict type")
}
