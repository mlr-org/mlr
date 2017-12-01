#' @export
makeRLearner.regr.nnet = function() {
  makeRLearnerRegr(
    cl = "regr.nnet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "size", default = 3L, lower = 0L),
      # FIXME nnet() seems to have no default for size, but if it is 3, par.vals is redundant
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      makeLogicalLearnerParam(id = "linout", default = FALSE, requires = quote(entropy == FALSE && softmax == FALSE && censored == FALSE)),
      makeLogicalLearnerParam(id = "entropy", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && censored == FALSE)),
      makeLogicalLearnerParam(id = "softmax", default = FALSE, requires = quote(entropy == FALSE && linout == FALSE && censored == FALSE)),
      makeLogicalLearnerParam(id = "censored", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && entropy == FALSE)),
      makeLogicalLearnerParam(id = "skip", default = FALSE),
      makeNumericLearnerParam(id = "rang", default = 0.7),
      makeNumericLearnerParam(id = "decay", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "Hess", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "MaxNWts", default = 1000L, lower = 1L, tunable = FALSE),
      makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
      makeNumericLearnerParam(id = "reltol", default = 1.0e-8)
    ),
    par.vals = list(size = 3L),
    properties = c("numerics", "factors", "weights"),
    name = "Neural Network",
    short.name = "nnet",
    note = "`size` has been set to `3` by default.",
    callees = "nnet"
  )
}

#' @export
trainLearner.regr.nnet = function(.learner, .task, .subset, .weights = NULL,  ...) {
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    nnet::nnet(f, data = getTaskData(.task, .subset), linout = TRUE, ...)
  } else  {
    f = getTaskFormula(.task)
    nnet::nnet(f, data = getTaskData(.task, .subset), linout = TRUE, weights = .weights, ...)
  }
}

#' @export
predictLearner.regr.nnet = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
