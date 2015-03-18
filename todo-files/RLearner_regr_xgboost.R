#' @export
makeRLearner.regr.xgboost = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = list("gbtree", "gblinear")),
      makeIntegerLearnerParam(id = "nthread", lower = 1L),
      makeIntegerLearnerParam(id = "nrounds", lower = 1L),
      makeNumericLearnerParam(id = "eta", lower = 0, default = 0.3, requires = expression(booster == "gbtree")),
      makeNumericLearnerParam(id = "gamma", lower = 0, requires = expression(booster == "gbtree")),
      makeIntegerLearnerParam(id = "max.depth", default = 6L, lower = 1L, requires = expression(booster == "gbtree")),
      makeIntegerLearnerParam(id = "min.child.weight", default = 1L, lower = 1L, requires = expression(booster == "gbtree")),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1, requires = expression(booster == "gbtree")),
      makeNumericLearnerParam(id = "colsample.bytree", default = 1, lower = 0, upper = 1, requires = expression(booster == "gbtree")),
      makeNumericLearnerParam(id = "lambda", default = 0, requires = expression(booster == "gblinear")),
      makeNumericLearnerParam(id = "alpha", default = 0, requires = expression(booster == "gblinear")),
      makeNumericLearnerParam(id = "lambda.bias", default = 0, requires = expression(booster == "gblinear"))
    ),
    par.vals = list(nrounds = 1L),
    properties = c("numerics"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgb",
    note = "`nround` should be set to value higher then 1 for more accuracy. `objective = 'multireg:linear'` set automatically for regression."
  )
}

#' @export
trainLearner.regr.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  task.data = getTaskData(.task, subset = .subset, target.extra = TRUE)
  d = task.data$data
  d = as.matrix(d)
  t = task.data$target
  d = xgboost::xgb.DMatrix(data = d, label = t)
  xgboost::xgb.train(data = d, objective = "reg:linear", ...)
}

#' @export
predictLearner.regr.xgboost = function(.learner, .model, .newdata, ...) {
  d = xgboost::xgb.DMatrix(data = as.matrix(.newdata))
  xgboost::predict(.model$learner.model, newdata = d, ...)
}
