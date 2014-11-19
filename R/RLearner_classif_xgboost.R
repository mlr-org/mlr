#' @export
makeRLearner.classif.xgboost = function() {
  makeRLearnerClassif(
    cl = "classif.xgboost",
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
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgb",
    note = "`nround` should be set to value higher then 1 for more accuracy. Mulitclass will use `objective = 'multi:softmax'`. Binaryclass will use `objective = 'binary:logistic'`."
  )
}

trainLearner.classif.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  task.data = getTaskData(.task, subset = .subset, target.extra = TRUE)
  d = task.data$data
  d = as.matrix(d)
  t = task.data$target
  t = as.integer(t) - 1
  k = length(.task$task.desc$class.levels)
  d = xgboost::xgb.DMatrix(data = d, label = t)
  if (k > 2){
    if (.learner$predict.type == "prob"){
      objective = "multi:softprob"
    } else {
      objective = "multi:softmax"
    }
    xgboost::xgb.train(data = d, objective = objective, num_class = k, ...)
  }
  else if (k == 2) {
    xgboost::xgb.train(data = d, objective = "binary:logistic", ...)
  }
}

predictLearner.classif.xgboost = function(.learner, .model, .newdata, ...) {
  d = xgb.DMatrix(data = as.matrix(.newdata))
  levs = .model$task.desc$class.levels
  # retourns 0,1 for binary, 0,1,2,... for multiclass
  pred = xgboost::predict(.model$learner.model, newdata = d, ...)
  if (.learner$predict.type == "prob") {
    if (length(levs) == 2) {
      res = propVectorToMatrix(pred, levs)
    } else {
      res = matrix(pred, ncol = length(levs), byrow = TRUE)
      colnames(res) = levs
    }
  } else {
    if (length(levs) == 2) {
      res = factor(ifelse(pred < 0.5, levs[1], levs[2]))
    } else {
      inds = as.integer(pred) + 1L
      # + 1 for index handling
      res = factor(levs[inds], levels = levs)
    }
  }
  res
}
