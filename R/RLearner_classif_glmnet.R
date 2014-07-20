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
      makeNumericLearnerParam(id = "threshold", default = 1e-07, lower = 0),
      makeIntegerLearnerParam(id = "dfmax", lower = 0L),
      makeIntegerLearnerParam(id = "pmax", lower = 0L),
      makeIntegerVectorLearnerParam(id = "exclude", lower = 1L),
      makeNumericVectorLearnerParam(id = "penalty.factor", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lower.limits", upper = 0),
      makeNumericVectorLearnerParam(id = "upper.limits", lower = 0),
      makeIntegerLearnerParam(id = "maxit", default = 10^5, lower = 1L),
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
      makeIntegerLearnerParam(id = "mxit", default = 100, lower = 1)
    ),
    properties = c("numerics", "prob", "twoclass", "multiclass", "weights"),
    par.vals = list(s = 0.01)
  )
}

#' @export
trainLearner.classif.glmnet = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  args = c(list(x = as.matrix(d$data), y = d$target), list(...))
  if (!is.null(.weights)) {
    args$weights = .weights
  }
  if (length(.task$task.desc$class.levels) == 2) {
    args$family = "binomial"
  } else {
    args$family = "multinomial"
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
predictLearner.classif.glmnet = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "prob"){
    p = predict(.model$learner.model, newx = as.matrix(.newdata), type = "response", ...)
    if (length(.model$task.desc$class.levels) == 2) {
      p = cbind(1 - p, p)
      colnames(p) = .model$task.desc$class.levels
    } else {
      p = p[,,1]
    }
  } else {
    p = predict(.model$learner.model, newx = as.matrix(.newdata), type = "class", ...)[,1]
    p = factor(p, .model$task.desc$class.levels)
  }
  p
}
