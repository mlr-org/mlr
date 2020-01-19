#' @export
makeRLearner.classif.adaboostm1 = function() {
  makeRLearnerClassif(
    cl = "classif.adaboostm1",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "P", default = 100L, lower = 90L),
      makeLogicalLearnerParam(id = "Q", default = FALSE),
      makeNumericLearnerParam(id = "S", default = 1L),
      makeIntegerLearnerParam(id = "I", default = 10L, lower = 1L),
      makeLogicalLearnerParam(id = "D", default = FALSE, requires = quote(!U)),
      makeUntypedLearnerParam(id = "W", default = list("DecisionStump")),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "ada Boosting M1",
    short.name = "adaboostm1",
    note = "NAs are directly passed to WEKA with `na.action = na.pass`.",
    callees = c("AdaBoostM1", "Weka_control")
  )
}

#' @export
trainLearner.classif.adaboostm1 = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  ctrl = RWeka::Weka_control(...)
  RWeka::AdaBoostM1(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.adaboostm1 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
