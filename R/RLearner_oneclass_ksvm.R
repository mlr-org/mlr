#' @export
makeRLearner.oneclass.ksvm = function() {
  makeRLearnerOneClass(
    cl = "oneclass.ksvm",
    package = "kernlab",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
      makeNumericLearnerParam(id = "nu", lower = 0, default = 0.2, requires = quote(type == "nu-svc" || type == "one-svc")),
      makeNumericLearnerParam(id = "sigma", lower = 0, requires = quote(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = quote(kernel %in% c("polydot", "anovadot", "besseldot"))),
      makeNumericLearnerParam(id = "scale", default = 1, lower = 0, requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "offset", default = 1, requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeIntegerLearnerParam(id = "order", default = 1L, requires = quote(kernel == "besseldot")),
      makeNumericLearnerParam(id = "tol", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeNumericVectorLearnerParam(id = "class.weights", len = NA_integer_, lower = 0),
      makeLogicalLearnerParam(id = "fit", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "cache", default = 40L, lower = 1L)
    ),
    properties =  c("oneclass", "numerics", "factors", "prob"),
    note = "'type' is set to 'one-svc'",
    name = "one-class kernlab-based SVM",
    short.name = "ksvm",
    callees = "ksvm"
  )
}

#' @export
trainLearner.oneclass.ksvm = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized, ...) {
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)

  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, .subset)[, x]
  # ksvm only support prob.model for C-svc, nu-svc and  C-bsvc not for one class
  if (base::length(kpar) > 0L){
    kernlab::ksvm(x = as.matrix(d), y = NULL, kpar = kpar, type = "one-svc", ...)
  }
  else {
    kernlab::ksvm(x = as.matrix(d), y = NULL, type = "one-svc", ...)
  }
}

#' @export
predictLearner.oneclass.ksvm = function(.learner, .model, .newdata, .truth = NULL, ...) {
  td = getTaskDesc(.model)
  label = c(td$positive, td$negative)
  if (.learner$predict.type == "response") {
    p = kernlab::predict(.model$learner.model, newdata = .newdata, type = "response", ...)
    p = factor(p, levels = c("FALSE", "TRUE"), labels = label)
  } else {
    p = kernlab::predict(.model$learner.model, newdata = .newdata, type = "decision", ...)
    p = convertingScoresToProbability(p, param = c(0, 1))$probability
    p = cbind(1 - p, p)
    colnames(p) = label
  }
  return(p)
}
