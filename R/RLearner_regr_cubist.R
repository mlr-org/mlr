#' @export
makeRLearner.regr.cubist = function() {
  makeRLearnerRegr(
    cl = "regr.cubist",
    package = "Cubist",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "committees", default = 1L, lower = 1L, upper = 100L),
      makeLogicalLearnerParam(id = "unbiased", default = FALSE),
      makeIntegerLearnerParam(id = "rules", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "extrapolation", default = 100, lower = 0, upper = 100),
      makeIntegerLearnerParam(id = "sample", default = 0L, lower = 0L),
      makeIntegerLearnerParam(id = "seed", default = sample.int(4096, size = 1) - 1L, tunable = FALSE),
      makeUntypedLearnerParam(id = "label", default = "outcome"),
      makeIntegerLearnerParam(id = "neighbors", default = 0L, lower = 0L, upper = 9L, when = "predict")
    ),
    properties = c("missings", "numerics", "factors"),
    name = "Cubist",
    short.name = "cubist",
    callees = c("cubist", "cubistControl", "predict.cubist")
  )
}

#' @export
trainLearner.regr.cubist = function(.learner, .task, .subset, .weights = NULL, unbiased, rules,
  extrapolation, sample, seed, label, ...) {
  ctrl = learnerArgsToControl(Cubist::cubistControl, unbiased, rules, extrapolation, sample, seed, label)
  d = getTaskData(.task, .subset, target.extra = TRUE)
  Cubist::cubist(x = d$data, y = d$target, control = ctrl, ...)
}

#' @export
predictLearner.regr.cubist = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}
