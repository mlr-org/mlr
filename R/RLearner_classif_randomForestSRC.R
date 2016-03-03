#' @export
makeRLearner.classif.randomForestSRC = function() {
  makeRLearnerClassif(
    cl = "classif.randomForestSRC",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none")),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "nsplit", default = 0L),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute"), when = "both"),
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeNumericVectorLearnerParam(id = "xwar.wt", lower = 0),
      makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "fast.restore", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors", "prob", "twoclass",
      "multiclass", "oob"),
    name = "Random Forest",
    short.name = "rfsrc",
    note = "`na.action` has been set to `na.impute` by default to allow missing data support."
  )
}

#' @export
trainLearner.classif.randomForestSRC = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  randomForestSRC::rfsrc(f, data = getTaskData(.task, .subset, recode.target = "drop.levels"), importance = "none", proximity = FALSE, forest = TRUE, ...)
}

#' @export
predictLearner.classif.randomForestSRC = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, importance = "none", ...)
  if (.learner$predict.type == "prob") {
    return(p$predicted)
  } else {
    return(p$class)
  }
}

getOutOfBag.classif.randomForestSRC = function(.learner, .model) {
  mod = .model$learner.model
  preds = as.data.frame(mod$predicted.oob)
  if (.learner$predict.type != "prob") {
    levs = names(preds)
    preds.class = as.integer(apply(preds, 1L, which.max))
    preds = as.factor(levs[preds.class])
  }
  err = mean(mod$err.rate[, 1L])
  list(response = preds, err = err)
}
