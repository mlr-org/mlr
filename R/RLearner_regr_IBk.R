#' @export
makeRLearner.regr.IBk = function() {
  makeRLearnerRegr(
    cl = "regr.IBk",
    package = "RWeka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "I"),
      makeLogicalLearnerParam(id = "F"),
      makeIntegerLearnerParam(id = "K", lower = 1L, default = 1L),
      makeLogicalLearnerParam(id = "E"),
      makeIntegerLearnerParam(id = "W", lower = 0L),
      makeLogicalLearnerParam(id = "X"),
      makeUntypedLearnerParam(id = "A", default = "weka.core.neighboursearch.LinearNNSearch")
    ),
    properties = c("numerics", "factors", "prob"),
    name = "K-nearest neighbours",
    short.name = "IBk",
    note = ""
  )
}

#' @export
trainLearner.regr.IBk = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = Weka_control(...)
  IBk(getTaskFormula(.task), data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.regr.IBk = function(.learner, .model, .newdata, ...) {
  # FIXME cannot be correct! prob?
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
