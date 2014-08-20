#' @export
makeRLearner.regr.pcr = function() {
  makeRLearnerRegr(
    cl = "regr.pcr",
    package = "pls",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ncomp", lower = 1L),
      makeDiscreteLearnerParam(id = "method", default = "cppls",
        values = c("kernelpls", "widekernelpls", "simpls", "oscorespls", "cppls", "svdpc")),
      makeLogicalLearnerParam(id = "scale", default = FALSE),
      makeLogicalLearnerParam(id = "model", default = TRUE),
      makeLogicalLearnerParam(id = "x", default = FALSE),
      makeLogicalLearnerParam(id = "y", default = FALSE)
    ),
    par.vals = list(model = FALSE),
    properties = c("numerics", "factors"),
    name = "Principal component regression",
    short.name = "pcr",
    note = "Note that `model` has been set to `FALSE` by default for speed."
  )
}

#' @export
trainLearner.regr.pcr = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  pcr(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.pcr = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata)
  p[, 1L, dim(p)[3L]]
}
