#' @export
makeRLearner.regr.rvm = function() {
  makeRLearnerRegr(
    cl = "regr.rvm",
    package = "kernlab",
    # to do: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
      makeNumericLearnerParam(id = "sigma",
        lower = 0, requires = quote(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L,
        requires = quote(kernel %in% c("polydot", "anovadot", "besseldot"))),
      makeNumericLearnerParam(id = "scale", default = 1, lower = 0,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "offset", default = 1,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "order", default = 1L,
        requires = quote(kernel == "besseldot")),
      makeNumericLearnerParam(id = "alpha", default = 5L, lower = 0L),
      makeNumericLearnerParam(id = "var", default = 0.1, lower = 0),
      makeLogicalLearnerParam(id = "var.fix", default = FALSE),
      makeNumericLearnerParam(id = "iterations", default = 100L, lower = 0L),
      makeNumericLearnerParam(id = "tol", default = .Machine$double.eps, lower = 0),
      makeNumericLearnerParam(id = "minmaxdiff", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "verbosity", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "fit", default = TRUE),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE)
    ),
    par.vals = list(fit = FALSE),
    properties = c("numerics", "factors"),
    name = "Relevance Vector Machine",
    short.name = "rvm",
    note = "Kernel parameters have to be passed directly and not by using the `kpar` list in `rvm`. Note that `fit` has been set to `FALSE` by default for speed.",
    callees = "rvm"
  )
}

#' @export
trainLearner.regr.rvm = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized, ...) {
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)
  f = getTaskFormula(.task)
  if (base::length(kpar)) {
    kernlab::rvm(f, data = getTaskData(.task, .subset), kpar = kpar, ...)
  } else {
    kernlab::rvm(f, data = getTaskData(.task, .subset), ...)
  }
}

#' @export
predictLearner.regr.rvm = function(.learner, .model, .newdata, ...) {
  kernlab::predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
