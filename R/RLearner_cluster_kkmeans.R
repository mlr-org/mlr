#' @export
makeRLearner.cluster.kkmeans = function() {
  makeRLearnerCluster(
    cl = "cluster.kkmeans",
    package = "kernlab",
    # FIXME: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "centers"),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
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
      makeDiscreteLearnerParam(id = "alg", values = c("kkmeans", "kerninghan"), default = "kkmeans"),
      # FIXME:  Not sure what p changes in the model, it didn't change anything in all test runs.
      makeNumericLearnerParam(id = "p", default = 1)
    ),
    par.vals = list(centers = 2L),
    properties = "numerics",
    name = "Kernel K-Means",
    note = "`centers` has been set to `2L` by default. The nearest center in kernel distance determines cluster assignment of new data points. Kernel parameters have to be passed directly and not by using the `kpar` list in `kkmeans`",
    short.name = "kkmeans",
    callees = "kkmeans"
  )
}

#' @export
trainLearner.cluster.kkmeans = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized, ...) {
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)
  if (base::length(kpar) > 0L) {
    kernlab::kkmeans(as.matrix(getTaskData(.task, .subset)), kpar = kpar, ...)
  } else {
    kernlab::kkmeans(as.matrix(getTaskData(.task, .subset)), ...)
  }
}

#' @export
predictLearner.cluster.kkmeans = function(.learner, .model, .newdata, .weights = NULL, ...) {

  c = kernlab::centers(.model$learner.model)
  K = kernlab::kernelf(.model$learner.model)

  # kernel product between each new datapoint and the centers
  d.xc = matrix(kernlab::kernelMatrix(K, as.matrix(.newdata), c), ncol = nrow(c))
  # kernel product between each new datapoint and itself: rows are identical
  d.xx = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(.newdata))), each = ncol(d.xc)), ncol = ncol(d.xc), byrow = TRUE)
  # kernel product between each center and itself: columns are identical
  d.cc = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(c))), each = nrow(d.xc)), nrow = nrow(d.xc))
  # this is the squared kernel distance to the centers
  d2 = d.xx + d.cc - 2 * d.xc
  # the nearest center determines cluster assignment
  res = apply(d2, 1, function(x) BBmisc::getMinIndex(x, ties.method = "random"))
  return(res)
}
