#' @export
makeRLearner.classif.xgboost = function() {
  makeRLearnerClassif(
    cl = "classif.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      # makeUntypedLearnerParam(id = "params", default = list()),
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
      makeUntypedLearnerParam(id = "objective", default = "binary:logistic", tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = "error", tunable = FALSE),
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
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights", "missings", "featimp"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default. `num_class` is set internally, so do not set this manually."
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

#' @export
getFeatureImportanceLearner.classif.xgboost = function(.learner, .model, ...) {
  mod = getLearnerModel(.model)
  imp = xgboost::xgb.importance(feature_names = .model$features,
                                model = mod, ...)

  fiv = imp$Gain
  setNames(fiv, imp$Feature)
}


