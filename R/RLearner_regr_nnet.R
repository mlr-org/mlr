#' @export
makeRLearner.regr.nnet = function() {
  makeRLearnerRegr(
    cl = "regr.nnet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "size", default = 3L, lower = 0L),
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      makeLogicalLearnerParam(id = "linout", default = FALSE, requires = expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "entropy", default = FALSE, requires = expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "softmax", default = FALSE, requires = expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "censored", default = FALSE, requires = expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
      makeLogicalLearnerParam(id = "skip", default = FALSE),
      makeNumericLearnerParam(id = "rang", default = 0.7),
      makeNumericLearnerParam(id = "decay", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "Hess", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE),
      makeIntegerLearnerParam(id = "MaxNWts", default = 1000L, lower = 1L),
      makeNumericLearnerParam(id = "abstoll", default = 1.0e-4),
      makeNumericLearnerParam(id = "reltoll", default = 1.0e-8)
    ),
    par.vals = list(size = 3L),
    properties = c("numerics", "factors", "weights"),
    name = "regr.nnet",
    short.name = "nnet",
    note = "Note that \\code{size} has been set to 3 by default."
  )
}

#' @export
trainLearner.regr.nnet = function(.learner, .task, .subset, .weights = NULL,  ...) {
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    nnet(f, data = getTaskData(.task, .subset), linout = TRUE, ...)
  } else  {
    f = as.formula(getTaskFormulaAsString(.task))
    nnet(f, data = getTaskData(.task, .subset), linout = TRUE, weights = .weights, ...)
  }
}

#' @export
predictLearner.regr.nnet = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
