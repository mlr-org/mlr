#' @export
makeRLearner.regr.xgboost = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      #makeUntypedLearnerParam(id = "params", default = list()),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear")),
      makeIntegerLearnerParam(id = "silent", default = 0L, tunable = FALSE),
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = "reg:linear", tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = "rmse", tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),

      makeNumericLearnerParam(id = "missing", default = NULL, tunable = FALSE, when = "both",
        special.vals = list(NA, NA_real_, NULL)),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "nrounds", default = 1L, lower = 1L),
      # FIXME nrounds seems to have no default in xgboost(), if it has 1, par.vals is redundant
      makeUntypedLearnerParam(id = "feval", default = NULL, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1L, lower = 0L, upper = 2L, tunable = FALSE),
      makeIntegerLearnerParam(id = "print.every.n", default = 1L, lower = 1L, tunable = FALSE,
        requires = quote(verbose == 1L)),
      makeIntegerLearnerParam(id = "early.stop.round", default = NULL, lower = 1L, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE)
    ),
    par.vals = list(nrounds = 1L, verbose = 0L),
    properties = c("numerics", "factors", "weights", "featimp"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default."
  )
}

#' @export
trainLearner.regr.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  target = data$target
  data = data.matrix(data$data)

  parlist = list(...)
  obj = parlist$objective
  if (is.null(obj)) {
    obj = "reg:linear"
  }

  if (is.null(.weights)) {
    xgboost::xgboost(data = data, label = target, objective = obj, ...)
  } else {
    xgb.dmat = xgboost::xgb.DMatrix(data = data, label = target, weight = .weights)
    xgboost::xgboost(data = xgb.dmat, label = NULL, objective = obj, ...)
  }
}

#' @export
predictLearner.regr.xgboost = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  xgboost::predict(m, newdata = data.matrix(.newdata), ...)
}

#' @export
getFeatureImportanceLearner.regr.xgboost = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.xgboost(.learner, .model, ...)
}
