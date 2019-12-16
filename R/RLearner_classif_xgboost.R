#' @export
makeRLearner.classif.xgboost = function() {
  makeRLearnerClassif(
    cl = "classif.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      # makeUntypedLearnerParam(id = "params", default = list()),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      makeUntypedLearnerParam(id = "watchlist", default = NULL, tunable = FALSE),
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 0L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 1, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = "binary:logistic", tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = "error", tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeNumericLearnerParam(id = "max_delta_step", lower = 0, default = 0),
      makeNumericLearnerParam(id = "missing", default = NA, tunable = FALSE, when = "both", special.vals = list(NA, NA_real_, NULL)),
      makeIntegerVectorLearnerParam(id = "monotone_constraints", default = 0, lower = -1, upper = 1),
      makeNumericLearnerParam(id = "tweedie_variance_power", lower = 1, upper = 2, default = 1.5, requires = quote(objective == "reg:tweedie")),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "nrounds", lower = 1L),
      makeUntypedLearnerParam(id = "feval", default = NULL, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1L, lower = 0L, upper = 2L, tunable = FALSE),
      makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L)),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = NULL, lower = 1L, special.vals = list(NULL), tunable = FALSE),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "scale_pos_weight", default = 1),
      makeLogicalLearnerParam(id = "refresh_leaf", default = TRUE),
      makeDiscreteLearnerParam(id = "feature_selector", default = "cyclic", values = c("cyclic", "shuffle", "random", "greedy", "thrifty")),
      makeIntegerLearnerParam(id = "top_k", default = 0, lower = 0),
      makeDiscreteLearnerParam(id = "predictor", default = "cpu_predictor", values = c("cpu_predictor", "gpu_predictor")),
      makeUntypedLearnerParam(id = "updater"), # Default depends on the selected booster
      makeNumericLearnerParam(id = "sketch_eps", default = 0.03, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "one_drop", default = FALSE, requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "tree_method", default = "auto", values = c("auto", "exact", "approx", "hist", "gpu_hist"), requires = quote(booster != "gblinear")),
      makeDiscreteLearnerParam(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), requires = quote(tree_method == "hist")),
      makeIntegerLearnerParam(id = "max_leaves", default = 0L, lower = 0L, requires = quote(grow_policy == "lossguide")),
      makeIntegerLearnerParam(id = "max_bin", default = 256L, lower = 2L, requires = quote(tree_method == "hist")),
      makeUntypedLearnerParam(id = "callbacks", default = list(), tunable = FALSE)
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
trainLearner.classif.xgboost = function(.learner, .task, .subset, .weights = NULL, ...) {

  td = getTaskDesc(.task)
  parlist = list(...)
  nc = length(td$class.levels)
  nlvls = length(td$class.levels)

  if (is.null(parlist$objective)) {
    parlist$objective = if (nlvls == 2L) "binary:logistic" else "multi:softprob"
  }

  if (.learner$predict.type == "prob" && parlist$objective == "multi:softmax") {
    stop("objective = 'multi:softmax' does not work with predict.type = 'prob'")
  }

  # if we use softprob or softmax as objective we have to add the number of classes 'num_class'
  if (parlist$objective %in% c("multi:softprob", "multi:softmax")) {
    parlist$num_class = nc
  }

  task.data = getTaskData(.task, .subset, target.extra = TRUE)
  label = match(as.character(task.data$target), td$class.levels) - 1

  # recode to 0:1 to that for the binary case the positive class translates to 1 (https://github.com/mlr-org/mlr3learners/issues/32)
  # task.data$target is guaranteed to have the factor levels in the right order
  label = nlvls - as.integer(task.data$target)
  parlist$data = xgboost::xgb.DMatrix(data = data.matrix(task.data$data), label = label)

  if (!is.null(.weights)) {
    xgboost::setinfo(parlist$data, "weight", .weights)
  }

  if (is.null(parlist$watchlist)) {
    parlist$watchlist = list(train = parlist$data)
  }

  do.call(xgboost::xgb.train, parlist)
}

#' @export
predictLearner.classif.xgboost = function(.learner, .model, .newdata, ...) {

  td = .model$task.desc
  m = .model$learner.model
  cls = rev(td$class.levels)
  nc = length(cls)
  obj = .learner$par.vals$objective
  nlvls = length(cls)

  if (is.null(obj)) {
    .learner$par.vals$objective = if (nlvls == 2L) "binary:logistic" else "multi:softprob"
  }

  p = predict(m, newdata = data.matrix(.newdata), ...)

  if (nc == 2L) { # binaryclass
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
  } else { # multiclass
    if (.learner$par.vals$objective == "multi:softmax") {
      p = as.factor(p) # special handling for multi:softmax which directly predicts class levels
      levels(p) = cls
      return(p)
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
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  imp = xgboost::xgb.importance(feature_names = .model$features,
    model = mod, ...)

  fiv = imp$Gain
  setNames(fiv, imp$Feature)
}
