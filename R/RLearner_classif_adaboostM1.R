# checked props
#' @export
makeRLearner.classif.adaboostM1 = function() {
  makeRLearnerClassif(
    cl = "classif.adaboostM1",
    package = c("RWeka", "rpart"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "P", default = 100L, lower = 90L),
      makeLogicalLearnerParam(id = "Q", default = FALSE),
      makeNumericLearnerParam(id = "S", default = 1L),
      makeIntegerLearnerParam(id = "I", default = 10L, lower = 1L),
      makeLogicalLearnerParam(id = "D", default = FALSE, requires = quote(!U)),
      makeUntypedLearnerParam(id = "W", default = RWeka::DecisionStump),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "adaboostM1 boosting Trees",
    short.name = "adaboostM1",
    note = "",
    callees = c("adaboostM1", "Weka_control")
  )
}

#' @export
trainLearner.classif.adaboostM1 = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  ctrl = RWeka::Weka_control(...)
  RWeka::AdaBoostM1(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.adaboostM1 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

