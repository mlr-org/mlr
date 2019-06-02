#' @export
makeRLearner.classif.lssvm = function() {
  makeRLearnerClassif(
    cl = "classif.lssvm",
    package = "kernlab",
    # to do: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
      makeNumericLearnerParam(id = "tau", lower = 0, default = 0.01),
      makeLogicalLearnerParam(id = "reduced", default = TRUE),
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
      makeNumericLearnerParam(id = "tol", default = 0.0001, lower = 0),
      makeLogicalLearnerParam(id = "fit", default = TRUE)
    ),
    par.vals = list(fit = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors"),
    name = "Least Squares Support Vector Machine",
    short.name = "lssvm",
    note = "`fitted` has been set to `FALSE` by default for speed.",
    callees = "lssvm"
  )
}

#' @export
trainLearner.classif.lssvm = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized, ...) {
  # FIXME: custom kernel. freezes? check mailing list
  # FIXME: unify cla + regr, test all sigma stuff

  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)
  f = getTaskFormula(.task)

  if (base::length(kpar)) {
    kernlab::lssvm(f, data = getTaskData(.task, .subset), kpar = kpar, ...)
  } else {
    kernlab::lssvm(f, data = getTaskData(.task, .subset), ...)
  }
}

#' @export
predictLearner.classif.lssvm = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, "response")
  kernlab::predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
