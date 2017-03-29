#' @export
makeRLearner.oneclass.ksvm = function() {
  makeRLearnerOneClass(
    cl = "oneclass.ksvm",
    package = "kernlab",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "type", default = "one-svc", values = c("one-svc")),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
      makeNumericLearnerParam(id = "C", lower = 0, default = 1, requires = quote(type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc"))),
      makeNumericLearnerParam(id = "nu", lower = 0, default = 0.2, requires = quote(type == "nu-svc")),
      makeNumericLearnerParam(id = "epsilon", default = 0.1, requires = quote(type %in% c("eps-svr", "nu-svr", "eps-bsvm"))),
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
    par.vals = list(type = "one-svc", fit = FALSE),
    properties =  c("oneclass", "numerics", "factors", "weights"),
    name = "one-class KSVM",
    short.name = "one-class ksvm"
  )
}

#' @export
trainLearner.oneclass.ksvm = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized, ...) {

  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)

  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, .subset)[, x]
  k = as.kernelMatrix(crossprod(t(d)))
  # ksvm only support prob.model for C-svc, nu-svc and  C-bsvc not for one class
  if (base::length(kpar) > 0L){
    m = kernlab::ksvm(x = k, y = NULL, kpar = kpar, ...)
    # need support vectors for prediction
    sv = d[SVindex(m),]
    list(model = m, sv = sv)
  }
  else {
    m = kernlab::ksvm(x = k, y = NULL, ...)
    # need support vectors for prediction
    sv = d[SVindex(m),]
    list(model = m, sv = sv)
  }
}

#' @export
predictLearner.oneclass.ksvm = function(.learner, .model, .newdata, .truth = NULL, ...) {
  # ksvm currently can't predict probabilities only response
  type = switch(.learner$predict.type, prob = "response")
  Ktest = as.kernelMatrix(crossprod(t(.newdata), t(.model$learner.model$sv)))
  kernlab::predict(.model$learner.model$model, newdata = Ktest, type = type, ...)
}


