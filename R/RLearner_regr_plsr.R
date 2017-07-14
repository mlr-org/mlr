#' @export
makeRLearner.regr.plsr = function() {
  makeRLearnerRegr(cl = "regr.plsr",
    package = "pls",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ncomp", lower = 1),
      makeDiscreteLearnerParam(id = "method", default = "kernelpls",
        values = c("kernelpls", "widekernelpls", "simpls", "oscorespls")),
      makeLogicalLearnerParam(id = "scale", default = TRUE),
      makeLogicalLearnerParam(id = "model", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "x", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "y", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors"),
    name = "Partial Least Squares Regression",
    short.name = "plsr",
    callees = "plsr"
  )
}

#' @export
trainLearner.regr.plsr = function(.learner, .task, .subset, .weights, ...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)
  pls::plsr(f, data = d, ...)
}

#' @export
predictLearner.regr.plsr = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, comps = seq_len(.model$learner.model$ncomp), ...)[, 1L]
}
