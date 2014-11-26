#' @export
makeRLearner.surv.randomForestSRC = function() {
  makeRLearnerSurv(
    cl = "surv.randomForestSRC",
    package = c("survival", "randomForestSRC"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", values = c("by.root", "by.node", "none"), default = "by.root"),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeNumericLearnerParam(id = "mtry.ratio", lower = 0L, upper = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 3L),
      makeDiscreteLearnerParam(id = "splitrule", values = c("logrank", "logrankscore"), default = "logrank"),
      makeDiscreteLearnerParam(id = "na.action", values = c("na.omit", "na.impute"), default = "na.omit")
    ),
    properties = c("missings", "numerics", "factors", "ordered", "rcens"),
    name = "Random Forests for Survival",
    short.name = "rfsrc",
    note = ""
  )
}

#' @export
trainLearner.surv.randomForestSRC = function(.learner, .task, .subset, .weights = NULL, mtry = NULL, mtry.ratio = NULL, ...) {
  data = getTaskData(.task, subset = .subset)
  if (!is.null(mtry.ratio)) {
    if (!is.null(mtry))
      stop("You cannot set both 'mtry' and 'mtry.ratio")
    mtry = mtry.ratio * nrow(data)
  }

  f = getTaskFormula(.task, env = as.environment("package:survival"))
  randomForestSRC::rfsrc(f, data = data, importance = "none", proximity = FALSE, forest = TRUE,
    mtry = mtry, ...)
}

#' @export
predictLearner.surv.randomForestSRC = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, importance = "none", na.action = "na.impute", ...)$predicted
  } else {
    stop("Unknown predict type")
  }
}
