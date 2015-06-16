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
      makeUntypedLearnerParam(id = "A", default = "weka.core.neighboursearch.LinearNNSearch"),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors"),
    name = "K-Nearest Neighbours",
    short.name = "ibk",
    note = ""
  )
}

#' @export
trainLearner.regr.IBk = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = RWeka::Weka_control(...)
  RWeka::IBk(getTaskFormula(.task), data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.regr.IBk = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, type = "class", ...)
}
