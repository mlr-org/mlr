#' @export
makeRLearner.regr.gausspr = function() {
  makeRLearnerRegr(
    cl = "regr.gausspr",
    package = "kernlab",
    # FIXME: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot",
          "besseldot", "anovadot", "splinedot")),
      makeNumericLearnerParam(id = "sigma",
        lower = 0, requires = quote(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L,
        requires = quote(kernel %in% c("polydot", "anovadot", "besseldot"))),
      makeNumericLearnerParam(id = "scale", default = 1, lower = 0,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "offset", default = 1,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeIntegerLearnerParam(id = "order", default = 1L,
        requires = quote(kernel == "besseldot")),
      makeNumericLearnerParam(id = "var", default = 0.001),
      makeNumericLearnerParam(id = "tol", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "fit", default = TRUE)
    ),
    par.vals = list(fit = FALSE),
    properties = c("numerics", "factors", "se"),
    name = "Gaussian Processes",
    short.name = "gausspr",
    note = "Kernel parameters have to be passed directly and not by using the `kpar` list in `gausspr`.
    Note that `fit` has been set to `FALSE` by default for speed.",
    callees = "gausspr"
  )
}

#' @export
trainLearner.regr.gausspr = function(.learner, .task, .subset, .weights = NULL,
  degree, offset, scale, sigma, order, length, lambda, normalized, ...) {
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)
  f = getTaskFormula(.task)
  vm = .learner$predict.type == "se"
  if (base::length(kpar) > 0L) {
    kernlab::gausspr(f, data = getTaskData(.task, .subset), kpar = kpar, variance.model = vm, type = "regression", ...)
  } else {
    kernlab::gausspr(f, data = getTaskData(.task, .subset), variance.model = vm, type = "regression", ...)
  }
}

#' @export
predictLearner.regr.gausspr = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type != "se") {
    as.vector(kernlab::predict(.model$learner.model, newdata = .newdata, ...))
  } else {
    pred = matrix(kernlab::predict(.model$learner.model, newdata = .newdata, ...))
    pred.se = matrix(kernlab::predict(.model$learner.model, newdata = .newdata, type = "sdeviation", ...))
    cbind(pred, pred.se)
  }
}
