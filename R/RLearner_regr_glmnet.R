#' @export
makeRLearner.regr.glmnet = function() {
  makeRLearnerRegr(
    cl = "regr.glmnet",
    package = "glmnet",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "s", default = 0.01, lower = 0, upper = 1, when = "predict"),
      makeLogicalLearnerParam(id = "exact", default = FALSE, when = "predict"),
      makeIntegerLearnerParam(id = "nlambda", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "lambda.min.ratio", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lambda"),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeLogicalLearnerParam(id = "intercept", default = TRUE),
      makeNumericLearnerParam(id = "threshold", default = 1e-07, lower = 0),
      makeIntegerLearnerParam(id = "dfmax", lower = 0L),
      makeIntegerLearnerParam(id = "pmax", lower = 0L),
      makeIntegerVectorLearnerParam(id = "exclude", lower = 1L),
      makeNumericVectorLearnerParam(id = "penalty.factor", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lower.limits", upper = 0),
      makeNumericVectorLearnerParam(id = "upper.limits", lower = 0),
      makeIntegerLearnerParam(id = "maxit", default = 10^5, lower = 1L),
      makeDiscreteLearnerParam(id = "type.gaussian", values = c("covariance","naive")),
      makeLogicalLearnerParam(id = "standardize.response", default = FALSE),
      makeNumericLearnerParam(id = "fdev", default = 1.0e-5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "devmax", default = 0.999, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "eps", default = 1.0e-6, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "big", default = 9.9e35),
      makeIntegerLearnerParam(id = "mnlam", default = 5, lower = 1),
      makeNumericLearnerParam(id = "pmin", default = 1.0e-9, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "exmx", default = 250.0),
      makeNumericLearnerParam(id = "prec", default = 1e-10),
      makeIntegerLearnerParam(id = "mxit", default = 100, lower = 1)
    ),
    properties = c("numerics", "prob", "twoclass", "multiclass", "weights"),
    par.vals = list(s = 0.01)
  )
}

#' @export
trainLearner.regr.glmnet = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  args = c(list(x = as.matrix(d$data), y = d$target, family = "gaussian"), list(...))
  if (!is.null(.weights)) {
    args$weights = .weights
  }
  ctrl.args = names(formals(glmnet.control))
  if (any(names(args) %in% ctrl.args)) {
    do.call(glmnet.control, args[names(args) %in% ctrl.args])
    mod = do.call(glmnet, args[!names(args) %in% ctrl.args])  
    glmnet.control(factory = TRUE)
  } else {
    mod = do.call(glmnet, args) 
  }
  mod
}

#' @export
predictLearner.regr.glmnet = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newx = as.matrix(.newdata), ...)[,1]
}
