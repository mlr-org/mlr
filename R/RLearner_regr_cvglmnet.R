#' @export
makeRLearner.regr.cvglmnet = function() {
  makeRLearnerRegr(
    cl = "regr.cvglmnet",
    package = "glmnet",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "nfolds", default = 10L, lower = 3L),
      makeDiscreteLearnerParam(id = "type.measure", values = c("deviance", "mse", "mae"), default = "deviance"),
      makeLogicalLearnerParam(id = "exact", default = FALSE, when = "predict"),
      # FIXME bug?
      # makeDiscreteLearnerParam(id = "s", values = c("lambda.1se", "lambda.min"), default = "lambda.1se", when = "predict"),
      makeIntegerLearnerParam(id = "nlambda", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "lambda.min.ratio", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lambda"),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeLogicalLearnerParam(id = "intercept", default = TRUE),
      makeNumericLearnerParam(id = "thresh", default = 1e-07, lower = 0),
      makeIntegerLearnerParam(id = "dfmax", lower = 0L),
      makeIntegerLearnerParam(id = "pmax", lower = 0L),
      makeIntegerVectorLearnerParam(id = "exclude", lower = 1L),
      makeNumericVectorLearnerParam(id = "penalty.factor", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lower.limits", upper = 0),
      makeNumericVectorLearnerParam(id = "upper.limits", lower = 0),
      makeIntegerLearnerParam(id = "maxit", default = 100000L, lower = 1L),
      makeDiscreteLearnerParam(id = "type.gaussian", values = c("covariance","naive")),
      makeLogicalLearnerParam(id = "standardize.response", default = FALSE),
      makeNumericLearnerParam(id = "fdev", default = 1.0e-5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "devmax", default = 0.999, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "eps", default = 1.0e-6, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "big", default = 9.9e35),
      makeIntegerLearnerParam(id = "mnlam", default = 5, lower = 1),
      makeNumericLearnerParam(id = "pmin", default = 1.0e-9, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "exmx", default = 250),
      makeNumericLearnerParam(id = "prec", default = 1e-10),
      makeIntegerLearnerParam(id = "mxit", default = 100L, lower = 1L)
    ),
    properties = c("numerics", "weights"),
    par.vals = list(s = 0.01),
    name = "GLM with lasso or elasticnet regularization",
    short.name = "cvglmnet",
    note = ""
  )
}

#' @export
trainLearner.regr.cvglmnet = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  args = c(list(x = as.matrix(d$data), y = d$target, family = "gaussian", parallel = FALSE), list(...))
  rm(d)
  if (!is.null(.weights))
    args$weights = .weights

  saved.ctrl = glmnet.control()
  is.ctrl.arg = names(args) %in% names(saved.ctrl)
  if (any(is.ctrl.arg)) {
    on.exit(do.call(glmnet.control, saved.ctrl))
    do.call(glmnet.control, args[is.ctrl.arg])
    args = args[!is.ctrl.arg]
  }

  do.call(cv.glmnet, args)
}

#' @export
predictLearner.regr.cvglmnet = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newx = as.matrix(.newdata), ...)[, 1L]
}
