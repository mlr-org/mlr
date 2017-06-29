#' @export
makeRLearner.regr.xgboost4customloss = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost4customloss",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      #makeUntypedLearnerParam(id = "params", default = list()),
      #makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
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
	  #makeIntegerLearnerParam(id = "silent", default = 0L, lower = 0L, upper = 1L, tunable = FALSE),
	  makeUntypedLearnerParam(id = "feval", default = NULL, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1L, lower = 0L, upper = 2L, tunable = FALSE),
      makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L)),
	  makeIntegerLearnerParam(id = "early_stopping_rounds", default = NULL, lower = 1L, special.vals = list(NULL), tunable = FALSE),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE)
      #makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      #makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      #makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart"))
      ),
    par.vals = list(nrounds = 1L, verbose = 1L),
    # properties = c("numerics", "factors", "weights"),
    properties = c("numerics", "weights", "featimp", "missings"),
    name = "eXtreme Gradient Boosting for custom loss",
    short.name = "xgboost4customloss",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default.",
    callees = "xgboost"
  )
}

# create xgboost train and predict methods for mlr package
trainLearner.regr.xgboost4customloss = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  target = data$target
  data = data.matrix(data$data)  # only contains feature now
  if (!is.null(.weights))
  	data = xgboost::xgb.DMatrix(data = data, label = target, weight = .weights)
  else
    data = xgboost::xgb.DMatrix(data = data, label = target)
  #data = FeatureHashing::hashed.model.matrix( ~ . - 1, data$data)

  myobj = function(preds, dtrain, c) {
    labels = xgboost::getinfo(dtrain, "label")
    x = preds-labels
    # introduce hyperparameter for objective function
    c = .learner$par.vals$obj_par
    grad = 2 * x / (labels + 10L) # broadcasting along the vector
    hess = 2 / (labels + 10L)
    #grad = tanh(c*x)
    #hess = c*sqrt(1-grad^2)
    return(list(grad = grad, hess = hess))
  }

  myobj2 = function(preds, dtrain, c) {
    labels = xgboost::getinfo(dtrain, "label")
    x = preds-labels
    # introduce hyperparameter for objective function
    c = .learner$par.vals$obj_par
    grad = 2 * x / (labels + 10L) # broadcasting along the vector
    hess = 2 / (labels + 10L)
    idx = which(labels > 70)
    grad[idx] = 2 * x[idx]
    hess[idx] = 2
    #grad = tanh(c*x)
    #hess = c*sqrt(1-grad^2)
    return(list(grad = grad, hess = hess))
  }

  myobj3 = function(preds, dtrain, c) {
    labels = xgboost::getinfo(dtrain, "label")
    x = preds-labels
    # introduce hyperparameter for objective function
    c = .learner$par.vals$obj_par
    grad = exp(abs(x)-70)
    grad = 2 * x / (labels + 10L) # broadcasting along the vector
    hess = 2 / (labels + 10L)
    idx = which(labels > 70)
    grad[idx] = 2 * x[idx]
    hess[idx] = 2
    #grad = tanh(c*x)
    #hess = c*sqrt(1-grad^2)
    return(list(grad = grad, hess = hess))
  }


  evalerror = function(preds, dtrain) {
    labels <- xgboost::getinfo(dtrain, "label")
    err <- sqrt(mean((preds-labels)^2))
    return(list(metric = "MSE", value = err))}
  xgboost::xgboost(data = data, label = target, objective = myobj2, eval_metric = evalerror, ...)
}
predictLearner.regr.xgboost4customloss = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  #data = FeatureHashing::hashed.model.matrix( ~ . - 1, .newdata)
  xgboost:::predict.xgb.Booster(m, newdata = data.matrix(.newdata), ...)
}

