#' @export
makeRLearner.classif.xgboost = function() {
  makeRLearnerClassif(
    cl = "classif.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      # makeUntypedLearnerParam(id = "params", default = list()),
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
      makeUntypedLearnerParam(id = "objective", default = "binary:logistic"),
      makeUntypedLearnerParam(id = "eval_metric", default = "error"),
      makeNumericLearnerParam(id = "base_score", default = 0.5),

      makeNumericLearnerParam(id = "missing", default = 0),
      makeIntegerLearnerParam(id = "nthread", default = 16,lower = 1),
      makeIntegerLearnerParam(id = "nrounds", default = 1, lower = 1),
      makeUntypedLearnerParam(id = "feval", default = NULL),
      makeIntegerLearnerParam(id = "verbose", default = 2, lower = 0, upper = 2),
      makeIntegerLearnerParam(id = "print.every.n", default = 1, lower = 1),
      makeIntegerLearnerParam(id = "early.stop.round", default = 1, lower = 1),
      makeLogicalLearnerParam(id = "maximize", default = TRUE)
    ),
    par.vals = list(nrounds = 1),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` by default."
  )
}

#' @export
trainLearner.classif.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  td = getTaskDescription(.task)
  data = getTaskData(.task, .subset, target.extra = TRUE)
  target = data$target
  target = match(as.character(target), td$class.levels) - 1
  data = data.matrix(data$data)

  if (length(td$class.levels) == 2L) {
    parlist = list(...)
    obj = parlist$objective
    if (is.null(obj)) {
      obj = "binary:logistic"
    }

    if (is.null(.weights)) {
      xgboost::xgboost(data = data, label = target, objective = obj, ...)
    } else {
      xgb.dmat = xgboost::xgb.DMatrix(data = data, label = target, weight = .weights)
      xgboost::xgboost(data = xgb.dmat, label = NULL, objective = obj, ...)
    }
  } else {
    parlist = list(...)
    obj = parlist$objective
    if (is.null(obj)) {
      obj = "multi:softprob"
    }
    num_class = length(td$class.levels)

    if (is.null(.weights)) {
      xgboost::xgboost(data = data, label = target, objective = obj, num_class = num_class, ...)
    } else {
      xgb.dmat = xgboost::xgb.DMatrix(data = data, label = target, weight = .weights)
      xgboost::xgboost(data = data, label = NULL, objective = obj, num_class = num_class, ...)
    }
  }
}

#' @export
predictLearner.classif.xgboost = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  m = .model$learner.model
  p = xgboost::predict(m, newdata = data.matrix(.newdata), ...)
  nc = length(td$class.levels)

  if (nc == 2L) {
    y = matrix(0, ncol = 2, nrow = nrow(.newdata))
    colnames(y) = td$class.levels
    y[, 1L] = 1-p
    y[, 2L] = p
    if (.learner$predict.type == "prob") {
      return(y)
    } else {
      p = colnames(y)[max.col(y)]
      names(p) = NULL
      p = factor(p, levels = colnames(y))
      return(p)
    }
  } else {
    nc = length(td$class.levels)
    p = matrix(p,nc,length(p)/nc)
    p = t(p)
    colnames(p) = td$class.levels
    if (.learner$predict.type == "prob") {
      return(p)
    } else {
      ind = max.col(p)
      cns = colnames(p)
      return(factor(cns[ind], levels = cns))
    }
  }
}
