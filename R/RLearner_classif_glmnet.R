#' @export
makeRLearner.classif.glmnet = function() {
  makeRLearnerClassif(
    cl = "classif.glmnet",
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
      makeIntegerLearnerParam(id = "mxit", default = 100L, lower = 1L)
    ),
    properties = c("numerics", "prob", "twoclass", "multiclass", "weights"),
    par.vals = list(s = 0.01),
    name = "GLM with Lasso or Elasticnet Regularization",
    short.name = "glmnet",
    note = ""
  )
}

#' @export
trainLearner.classif.glmnet = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  args = c(list(x = as.matrix(d$data), y = d$target), list(...))
  rm(d)
  if (!is.null(.weights))
    args$weights = .weights

  args$family = ifelse(length(.task$task.desc$class.levels) == 2L, "binomial", "multinomial")

  saved.ctrl = glmnet.control()
  is.ctrl.arg = names(args) %in% names(saved.ctrl)
  if (any(is.ctrl.arg)) {
    on.exit(do.call(glmnet.control, saved.ctrl))
    do.call(glmnet.control, args[is.ctrl.arg])
    args = args[!is.ctrl.arg]
  }

  do.call(glmnet, args)
}

#' @export
predictLearner.classif.glmnet = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "prob"){
    p = predict(.model$learner.model, newx = as.matrix(.newdata), type = "response",  ...)
    if (length(.model$task.desc$class.levels) == 2) {
      p = setColNames(cbind(1 - p, p), .model$task.desc$class.levels)
    } else {
      p = p[,,1]
    }
  } else {
    p = drop(predict(.model$learner.model, newx = as.matrix(.newdata), type = "class", ...))
    p = factor(p, .model$task.desc$class.levels)
  }
  p
}
