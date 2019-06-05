#' @export
makeRLearner.classif.ksvm = function() {
  makeRLearnerClassif(
    cl = "classif.ksvm",
    package = "kernlab",
    # FIXME: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "type", default = "C-svc", values = c("C-svc", "nu-svc", "C-bsvc", "spoc-svc", "kbb-svc")),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
      makeNumericLearnerParam(id = "C",
        lower = 0, default = 1, requires = quote(type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc"))),
      makeNumericLearnerParam(id = "nu",
        lower = 0, default = 0.2, requires = quote(type == "nu-svc")),
      makeNumericLearnerParam(id = "epsilon", default = 0.1,
        requires = quote(type %in% c("eps-svr", "nu-svr", "eps-bsvm"))),
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
      makeNumericVectorLearnerParam(id = "class.weights", len = NA_integer_, lower = 0),
      makeLogicalLearnerParam(id = "fit", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "cache", default = 40L, lower = 1L)
    ),
    par.vals = list(fit = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "class.weights",
    name = "Support Vector Machines",
    short.name = "ksvm",
    note = "Kernel parameters have to be passed directly and not by using the `kpar` list in `ksvm`. Note that `fit` has been set to `FALSE` by default for speed.",
    callees = "ksvm"
  )
}

#' @export
trainLearner.classif.ksvm = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized, ...) {

  # FIXME: custom kernel. freezes? check mailing list
  # FIXME: unify cla + regr, test all sigma stuff

  #     # there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
  #     if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
  #       args$kernel = do.call(args$kernel, kpar)
  #     }
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)
  f = getTaskFormula(.task)
  pm = .learner$predict.type == "prob"
  if (base::length(kpar) > 0L) {
    kernlab::ksvm(f, data = getTaskData(.task, .subset), kpar = kpar, prob.model = pm, ...)
  } else {
    kernlab::ksvm(f, data = getTaskData(.task, .subset), prob.model = pm, ...)
  }
}

#' @export
predictLearner.classif.ksvm = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "probabilities", "response")
  kernlab::predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
