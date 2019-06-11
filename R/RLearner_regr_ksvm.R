#' @export
makeRLearner.regr.ksvm = function() {
  makeRLearnerRegr(
    cl = "regr.ksvm",
    package = "kernlab",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "type", default = "eps-svr", values = c("eps-svr", "nu-svr", "eps-bsvr")),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
      makeNumericLearnerParam(id = "C",
        lower = 0, default = 1, requires = quote(type %in% c("eps-svr", "eps-bsvr"))),
      makeNumericLearnerParam(id = "nu",
        lower = 0, default = 0.2, requires = quote(type == "nu-svr")),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 0.1,
        requires = quote(type %in% c("eps-svr", "nu-svr", "eps-bsvr"))),
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
      makeNumericLearnerParam(id = "tol", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeLogicalLearnerParam(id = "fit", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "cache", default = 40L, lower = 1L)
    ),
    par.vals = list(fit = FALSE),
    properties = c("numerics", "factors"),
    name = "Support Vector Machines",
    short.name = "ksvm",
    note = "Kernel parameters have to be passed directly and not by using the `kpar` list in `ksvm`. Note that `fit` has been set to `FALSE` by default for speed.",
    callees = "ksvm"
  )
}

#' @export
trainLearner.regr.ksvm = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, ...) {
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda)
  f = getTaskFormula(.task)
  # difference in missing(kpar) and kpar = list()!
  if (base::length(kpar)) {
    kernlab::ksvm(f, data = getTaskData(.task, .subset), kpar = kpar, ...)
  } else {
    kernlab::ksvm(f, data = getTaskData(.task, .subset), ...)
  }
}

#' @export
predictLearner.regr.ksvm = function(.learner, .model, .newdata, ...) {
  kernlab::predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
