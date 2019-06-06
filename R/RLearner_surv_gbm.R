#' @export
makeRLearner.surv.gbm = function() {
  makeRLearnerSurv(
    cl = "surv.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "distribution", values = c("bernoulli", "adaboost", "huberized", "multinomial")),
      makeIntegerLearnerParam(id = "n.trees", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "cv.folds", default = 0L),
      makeIntegerLearnerParam(id = "interaction.depth", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "n.minobsinnode", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "shrinkage", default = 0.001, lower = 0),
      makeNumericLearnerParam(id = "bag.fraction", default = 0.5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "train.fraction", default = 1, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "keep.data", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    properties = c("missings", "numerics", "factors", "weights", "featimp"),
    par.vals = list(keep.data = FALSE),
    name = "Gradient Boosting Machine",
    short.name = "gbm",
    note = "`keep.data` is set to FALSE to reduce memory requirements.",
    callees = "gbm"
  )
}

#' @export
trainLearner.surv.gbm = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task)

  if (is.null(.weights)) {
    gbm::gbm(f, data = d, distribution = "coxph", ...)
  } else {
    gbm::gbm(f, data = d, weights = .weights, distribution = "coxph", ...)
  }
}

#' @export
predictLearner.surv.gbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  gbm::predict.gbm(m, newdata = .newdata, type = "response", n.trees = m$n.trees, single.tree = FALSE, ...)
}

#' @export
getFeatureImportanceLearner.surv.gbm = function(.learner, .model, ...) {
  mod = getLearnerModel(.model)
  gbm::relative.influence(mod, mod$n.trees, ...)
}
