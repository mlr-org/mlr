#' @export
makeRLearner.surv.cvglmnet = function() {
  makeRLearnerSurv(
    cl = "surv.cvglmnet",
    package = "glmnet",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "nfolds", default = 10L, lower = 3L),
      makeDiscreteLearnerParam(id = "type.measure", values = "deviance", default = "deviance"),
      makeLogicalLearnerParam(id = "grouped", default = TRUE),
      makeDiscreteLearnerParam(id = "s", values = c("lambda.1se", "lambda.min"), default = "lambda.1se", when = "predict"),
      makeIntegerLearnerParam(id = "nlambda", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "lambda.min.ratio", lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeLogicalLearnerParam(id = "intercept", default = TRUE),
      makeNumericLearnerParam(id = "thresh", default = 1e-07, lower = 0),
      makeIntegerLearnerParam(id = "dfmax", lower = 0L),
      makeIntegerLearnerParam(id = "pmax", lower = 0L),
      makeIntegerVectorLearnerParam(id = "exclude", lower = 1L), # this is like a subset?
      makeNumericVectorLearnerParam(id = "penalty.factor", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lower.limits", upper = 0),
      makeNumericVectorLearnerParam(id = "upper.limits", lower = 0),
      makeIntegerLearnerParam(id = "maxit", default = 100000L, lower = 1L),
      makeNumericLearnerParam(id = "fdev", default = 1.0e-5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "devmax", default = 0.999, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "eps", default = 1.0e-6, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "big", default = 9.9e35),
      makeIntegerLearnerParam(id = "mnlam", default = 5, lower = 1),
      makeNumericLearnerParam(id = "pmin", default = 1.0e-9, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "exmx", default = 250.0),
      makeNumericLearnerParam(id = "prec", default = 1e-10),
      makeIntegerLearnerParam(id = "mxit", default = 100, lower = 1),
      makeLogicalLearnerParam(id = "factory", default = FALSE),
      makeUntypedLearnerParam(id = "offset", default = NULL),
      makeDiscreteLearnerParam(id = "type.gaussian", values = c("covariance", "naive"), requires = quote(family == "gaussian")),
      makeNumericVectorLearnerParam(id = "gamma", lower = 0, upper = 1, requires = quote(relax == TRUE)),
      makeLogicalLearnerParam(id = "relax", default = FALSE)
    ),
    properties = c("numerics", "factors", "ordered", "weights"),
    name = "GLM with Regularization (Cross Validated Lambda)",
    short.name = "cvglmnet",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer.",
    callees = c("cv.glmnet", "glmnet", "glmnet.control", "predict.glmnet")
  )
}

#' @export
trainLearner.surv.cvglmnet = function(.learner, .task, .subset, .weights = NULL, ...) {

  d = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "surv")
  info = getFixDataInfo(d$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  args = c(list(x = as.matrix(fixDataForLearner(d$data, info)), y = d$target, family = "cox", parallel = FALSE), list(...))
  rm(d)
  if (!is.null(.weights)) {
    args$weights = .weights
  }

  saved.ctrl = glmnet::glmnet.control()
  is.ctrl.arg = names(args) %in% names(saved.ctrl)
  if (any(is.ctrl.arg)) {
    on.exit(do.call(glmnet::glmnet.control, saved.ctrl))
    do.call(glmnet::glmnet.control, args[is.ctrl.arg])
    args = args[!is.ctrl.arg]
  }

  attachTrainingInfo(do.call(glmnet::cv.glmnet, args), info)
}

#' @export
predictLearner.surv.cvglmnet = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  as.numeric(predict(.model$learner.model, newx = .newdata, type = "link", ...))
}
