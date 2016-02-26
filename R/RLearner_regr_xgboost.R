#' @export
makeRLearner.regr.xgboost = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      #makeUntypedLearnerParam(id = "params", default = list()),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear")),
      makeIntegerLearnerParam(id = "silent", default = 0),
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6, lower = 0),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1, lower = 1),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = "reg:linear"),
      makeUntypedLearnerParam(id = "eval_metric", default = "rmse"),
      makeNumericLearnerParam(id = "base_score", default = 0.5),

      makeNumericLearnerParam(id = "missing", default = 0),
      makeIntegerLearnerParam(id = "nthread", default = 16, lower = 1),
      makeIntegerLearnerParam(id = "nrounds", default = 1, lower = 1),
      makeUntypedLearnerParam(id = "feval", default = NULL),
      makeIntegerLearnerParam(id = "verbose", default = 1, lower = 0, upper = 2),
      makeIntegerLearnerParam(id = "print.every.n", default = 1, lower = 1),
      makeIntegerLearnerParam(id = "early.stop.round", default = 1, lower = 1),
      makeLogicalLearnerParam(id = "maximize", default = FALSE)
    ),
    par.vals = list(nrounds = 1),
    properties = c("numerics", "factors", "weights"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` by default."
  )
}

#' @export
trainLearner.regr.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  td = getTaskDescription(.task)
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
  td = .model$task.desc
  m = .model$learner.model
  xgboost::predict(m, newdata = data.matrix(.newdata), ...)
}
