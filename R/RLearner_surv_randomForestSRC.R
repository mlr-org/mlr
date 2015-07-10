#' @export
makeRLearner.surv.randomForestSRC = function() {
  makeRLearnerSurv(
    cl = "surv.randomForestSRC",
    package = c("survival", "randomForestSRC"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none")),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeNumericLearnerParam(id = "mtry.ratio", lower = 0L, upper = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 3L),
      makeDiscreteLearnerParam(id = "splitrule", default = "logrank",
        values = c("logrank", "logrankscore")),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute"), when = "both"),
      makeNumericVectorLearnerParam(id = "xwar.wt", lower = 0),
      makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "fast.restore", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors", "ordered", "rcens"),
    name = "Random Forests for Survival",
    short.name = "rfsrc",
    note = "'na.action' has been set to 'na.impute' by default to allow missing data support"
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

  f = getTaskFormula(.task)
  randomForestSRC::rfsrc(f, data = data, importance = "none", proximity = FALSE, forest = TRUE,
    mtry = mtry, ...)
}

#' @export
predictLearner.surv.randomForestSRC = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, importance = "none", ...)$predicted
  } else {
    stop("Unknown predict type")
  }
}
