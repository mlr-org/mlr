#' @export
makeRLearner.regr.gbm = function() {
  makeRLearnerRegr(
    cl = "regr.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "distribution", default = "gaussian", values = c("gaussian", "laplace", "poisson", "tdist", "quantile")),
      # FIXME default for distribution in gbm() is bernoulli
      makeIntegerLearnerParam(id = "n.trees", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "cv.folds", default = 0L),
      makeIntegerLearnerParam(id = "interaction.depth", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "n.minobsinnode", default = 10L, lower = 1L),
      makeNumericLearnerParam(id = "shrinkage", default = 0.001, lower = 0),
      makeNumericLearnerParam(id = "bag.fraction", default = 0.5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "train.fraction", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "alpha", default = 0.5, lower = 0, upper = 1,
                              requires = quote(distribution == "quantile")),
      makeLogicalLearnerParam(id = "keep.data", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(distribution = "gaussian", keep.data = FALSE),
    properties = c("missings", "numerics", "factors", "weights", "featimp"),
    name = "Gradient Boosting Machine",
    short.name = "gbm",
    note = '`keep.data` is set to FALSE to reduce memory requirements, `distribution` has been set to `"gaussian"` by default.',
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
