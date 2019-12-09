#' @export
makeRLearner.regr.gbm = function() {
  makeRLearnerRegr(
    cl = "regr.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "distribution", default = "gaussian", values = c("gaussian", "bernoulli", "huberized", "adaboost", "coxph", "pairwise", "laplace", "poisson", "tdist", "quantile")),
      makeIntegerLearnerParam(id = "n.trees", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "cv.folds", default = 0L),
      makeIntegerLearnerParam(id = "interaction.depth", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "n.minobsinnode", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "shrinkage", default = 0.1, lower = 0),
      makeNumericLearnerParam(id = "bag.fraction", default = 0.5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "train.fraction", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "alpha", default = 0.5, lower = 0, upper = 1,
        requires = quote(distribution == "quantile")),
      makeLogicalLearnerParam(id = "keep.data", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "n.cores", default = 1, tunable = FALSE)
    ),
    par.vals = list(distribution = "gaussian", keep.data = FALSE),
    properties = c("missings", "numerics", "factors", "weights", "featimp"),
    name = "Gradient Boosting Machine",
    short.name = "gbm",
    note = paste0(collapse = "", c('`keep.data` is set to FALSE to reduce memory requirements, `distribution` has been set to `"gaussian"` by default.',
      "Param 'n.cores' has been to set to '1' by default to suppress parallelization by the package.")),
    callees = "gbm"
  )
}

#' @export
trainLearner.regr.gbm = function(.learner, .task, .subset, .weights = NULL, ...) {

  f = getTaskFormula(.task)

  params = list(...)
  if ("alpha" %in% names(params)) {
    alpha = params$alpha
    params$alpha = NULL
  } else {
    alpha = 0.5
  }
  if (params$distribution == "quantile") {
    params$distribution = list(name = "quantile", alpha = alpha)
  }
  params$formula = f
  params$data = getTaskData(.task, .subset)

  if (!is.null(.weights)) {
    params$weights = .weights
  }

  do.call(gbm::gbm, params)
}

#' @export
predictLearner.regr.gbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  gbm::predict.gbm(m, newdata = .newdata, n.trees = length(m$trees), ...)
}

#' @export
getFeatureImportanceLearner.regr.gbm = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.gbm(.learner, .model, ...)
}
