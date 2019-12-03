#' @export
makeRLearner.classif.glmnet = function() {
  makeRLearnerClassif(
    cl = "classif.glmnet",
    package = "glmnet",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "s", lower = 0, when = "predict"),
      makeLogicalLearnerParam(id = "exact", default = FALSE, when = "predict"),
      makeIntegerLearnerParam(id = "nlambda", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "lambda.min.ratio", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lambda", lower = 0),
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
      makeDiscreteLearnerParam(id = "type.logistic", values = c("Newton", "modified.Newton")),
      makeDiscreteLearnerParam(id = "type.multinomial", values = c("ungrouped", "grouped")),
      makeNumericLearnerParam(id = "fdev", default = 1.0e-5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "devmax", default = 0.999, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "eps", default = 1.0e-6, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "big", default = 9.9e35),
      makeIntegerLearnerParam(id = "mnlam", default = 5, lower = 1),
      makeNumericLearnerParam(id = "pmin", default = 1.0e-9, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "exmx", default = 250.0),
      makeNumericLearnerParam(id = "prec", default = 1e-10),
      makeIntegerLearnerParam(id = "mxit", default = 100L, lower = 1L),
      makeUntypedLearnerParam(id = "offset", default = NULL),
      makeDiscreteLearnerParam(id = "type.gaussian", values = c("covariance", "naive"), requires = quote(family == "gaussian")),
      makeLogicalLearnerParam(id = "relax", default = FALSE)
    ),
    properties = c("numerics", "factors", "prob", "twoclass", "multiclass", "weights"),
    par.vals = list(s = 0.01),
    name = "GLM with Lasso or Elasticnet Regularization",
    short.name = "glmnet",
    note =
      "The family parameter is set to `binomial` for two-class problems and to `multinomial` otherwise.
      Factors automatically get converted to dummy columns, ordered factors to integer.
      Parameter `s` (value of the regularization parameter used for predictions) is set to `0.1` by default,
      but needs to be tuned by the user.
      glmnet uses a global control object for its parameters. mlr resets all control parameters to their defaults
      before setting the specified parameters and after training.
      If you are setting glmnet.control parameters through glmnet.control,
      you need to save and re-set them after running the glmnet learner.",
    callees = c("glmnet", "glmnet.control", "predict.glmnet")
  )
}

#' @export
trainLearner.classif.glmnet = function(.learner, .task, .subset, .weights = NULL, ...) {

  d = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "drop.levels")
  info = getFixDataInfo(d$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  args = c(list(x = as.matrix(fixDataForLearner(d$data, info)), y = d$target), list(...))
  rm(d)
  if (!is.null(.weights)) {
    args$weights = .weights
  }

  td = getTaskDesc(.task)
  args$family = ifelse(length(td$class.levels) == 2L, "binomial", "multinomial")

  glmnet::glmnet.control(factory = TRUE)
  saved.ctrl = glmnet::glmnet.control()
  is.ctrl.arg = names(args) %in% names(saved.ctrl)
  if (any(is.ctrl.arg)) {
    on.exit(glmnet::glmnet.control(factory = TRUE))
    do.call(glmnet::glmnet.control, args[is.ctrl.arg])
    args = args[!is.ctrl.arg]
  }

  attachTrainingInfo(do.call(glmnet::glmnet, args), info)
}

#' @export
predictLearner.classif.glmnet = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  if (.learner$predict.type == "prob") {
    p = predict(.model$learner.model, newx = .newdata, type = "response", ...)
    td = .model$task.desc
    if (length(td$class.levels) == 2L) {
      p = setColNames(cbind(1 - p, p), td$class.levels)
    } else {
      p = array(c(p), dim(p)[-3], dimnames = dimnames(p)[1:2])
    }
  } else {
    p = drop(predict(.model$learner.model, newx = .newdata, type = "class", ...))
    p = as.factor(p)
  }
  p
}
