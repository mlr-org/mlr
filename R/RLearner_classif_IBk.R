#' @export
makeRLearner.classif.IBk = function() {
  makeRLearnerClassif(
    cl = "classif.IBk",
    package = "RWeka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "I", default = FALSE),
      makeLogicalLearnerParam(id = "F", default = FALSE),
      makeIntegerLearnerParam(id = "K", lower = 1L, default = 1L),
      makeLogicalLearnerParam(id = "E", default = FALSE),
      makeIntegerLearnerParam(id = "W", lower = 0L),
      makeLogicalLearnerParam(id = "X", default = FALSE),
      makeUntypedLearnerParam(id = "A", default = "weka.core.neighboursearch.LinearNNSearch"),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "k-Nearest Neighbours",
    short.name = "ibk",
    callees = c("IBk", "Weka_control")
  )
}

#' @export
trainLearner.classif.IBk = function(.learner, .task, .subset, .weights = NULL, ...) {
  ctrl = RWeka::Weka_control(...)
  RWeka::IBk(getTaskFormula(.task), data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.IBk = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
