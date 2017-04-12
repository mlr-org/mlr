#' @export
makeRLearner.classif.xgboost = function() {
  makeRLearnerClassif(
    cl = "classif.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      # makeUntypedLearnerParam(id = "params", default = list()),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
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
      makeNumericLearnerParam(id = "max_delta_step", lower = 0, default = 0),
      makeNumericLearnerParam(id = "missing", default = NULL, tunable = FALSE, when = "both",
        special.vals = list(NA, NA_real_, NULL)),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "nrounds", default = 1L, lower = 1L),
      # FIXME nrounds seems to have no default in xgboost(), if it has 1, par.vals is redundant
      makeUntypedLearnerParam(id = "feval", default = NULL, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1L, lower = 0L, upper = 2L, tunable = FALSE),
      makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE,
        requires = quote(verbose == 1L)),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = NULL, lower = 1L, special.vals = list(NULL), tunable = FALSE),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart"))
    ),
    par.vals = list(nrounds = 1L, verbose = 0L),
    properties = c("twoclass", "multiclass", "numerics", "prob", "weights", "missings", "featimp"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default. `num_class` is set internally, so do not set this manually.",
    callees = "xgboost"
  )
}

#' @export
trainLearner.classif.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {

  td = getTaskDesc(.task)
  parlist = list(...)
  parlist$data = data.matrix(getTaskData(.task, .subset, target.extra = TRUE)$data)
  parlist$label = match(as.character(getTaskData(.task, .subset, target.extra = TRUE)$target), td$class.levels) - 1
  nc = length(td$class.levels)

  if (is.null(parlist$objective))
    parlist$objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")

  if (.learner$predict.type == "prob" && parlist$objective == "multi:softmax")
    stop("objective = 'multi:softmax' does not work with predict.type = 'prob'")

  #if we use softprob or softmax as objective we have to add the number of classes 'num_class'
  if (parlist$objective %in% c("multi:softprob", "multi:softmax"))
    parlist$num_class = nc

  if (!is.null(.weights))
    parlist$data = xgboost::xgb.DMatrix(data = parlist$data, label = parlist$label, weight = .weights)

  do.call(xgboost::xgboost, parlist)
}

#' @export
predictLearner.classif.xgboost = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  m = .model$learner.model
  cls = td$class.levels
  nc = length(cls)
  obj = .learner$par.vals$objective

  if (is.null(obj))
    .learner$par.vals$objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")

  p = predict(m, newdata = data.matrix(.newdata), ...)

  if (nc == 2L) { #binaryclass
    if (.learner$par.vals$objective == "multi:softprob") {
      y = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
      colnames(y) = cls
    } else {
      y = matrix(0, ncol = 2, nrow = nrow(.newdata))
      colnames(y) = cls
      y[, 1L] = 1 - p
      y[, 2L] = p
    }
    if (.learner$predict.type == "prob") {
      return(y)
    } else {
      p = colnames(y)[max.col(y)]
      names(p) = NULL
      p = factor(p, levels = colnames(y))
      return(p)
    }
  } else { #multiclass
    if (.learner$par.vals$objective  == "multi:softmax") {
      return(factor(p, levels = cls)) #special handling for multi:softmax which directly predicts class levels
    } else {
      p = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
      colnames(p) = cls
      if (.learner$predict.type == "prob") {
        return(p)
      } else {
        ind = max.col(p)
        cns = colnames(p)
        return(factor(cns[ind], levels = cns))
      }
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


