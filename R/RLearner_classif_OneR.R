#' @export
makeRLearner.classif.OneR = function() {
  makeRLearnerClassif(
    cl = "classif.OneR",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "B", default = 6L, lower = 1L),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "1-R Classifier",
    short.name = "oner",
    note = "NAs are directly passed to WEKA with `na.action = na.pass`.",
    callees = c("OneR", "Weka_control")
  )
}

#' @export
trainLearner.classif.OneR = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  ctrl = RWeka::Weka_control(...)
  RWeka::OneR(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.OneR = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
