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
    note = "Kernel parameters have to be passed directly and not by using the `kpar` list in `kkmeans`",
    short.name = "kkmeans"
  )
}

#' @export
trainLearner.cluster.kkmeans = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized, ...) {
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)
  if (base::length(kpar) > 0L)
    kernlab::kkmeans(as.matrix(getTaskData(.task, .subset)), kpar = kpar, ...)
  else
    kernlab::kkmeans(as.matrix(getTaskData(.task, .subset)), ...)
}

#' @export
predictLearner.cluster.kkmeans = function(.learner, .model, .newdata, .weights = NULL, ...) {
  c = kernlab::centers(.model$learner.model)
  K = kernlab::kernelf(.model$learner.model)
  
  # kernel product between each new datapoint and the centers
  Dxc = matrix(kernlab::kernelMatrix(K, as.matrix(.newdata), c), ncol = nrow(c))
  # kernel product between each new datapoint and itself: rows are identical
  Dxx = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(.newdata))), each = ncol(Dxc)), ncol = ncol(Dxc), byrow = TRUE)
  # kernel product between each center and itself: columns are identical
  Dcc = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(c))), each = nrow(Dxc)), nrow = nrow(Dxc))
  # this is the squared kernel distance to the centers
  D2 = Dxx + Dcc - 2*Dxc
  # the nearest center determines cluster assignment
  res = apply(D2, 1, which.min)
  return(res)
}
