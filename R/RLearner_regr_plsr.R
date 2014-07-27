#' @export
makeRLearner.regr.plsr = function() {
  makeRLearnerRegr(cl = "regr.plsr",
    package = "pls",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ncomp", lower = 1),
      makeDiscreteLearnerParam(id = "method", default = "kernelpls", 
        values = c("kernelpls", "widekernelpls", "simpls", "oscorespls")),
      makeLogicalLearnerParam(id = "scale", default = TRUE)
    ),
    properties = c("numerics", "factors"))
}

#' @export
trainLearner.regr.plsr = function(.learner, .task, .subset, .weights, ...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)
  plsr(f, data = d, ...)
}

#' @export
predictLearner.regr.plsr = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, comps = 1:.model$learner.model$ncomp, ...)[,1]
}
