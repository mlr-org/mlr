# checked props
#' @export
makeRLearner_classif_adaboostM1 = function() {
  makeRLearnerClassif(
    cl = "classif.adaboostM1",
    package = "RWeka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "P", default = 100L,lower =90),
      makeLogicalLearnerParam(id = "Q", default = FALSE),
      makeNumericLearnerParam(id = "S", default = 1L),
      makeIntegerLearnerParam(id = "I", default = 10L, lower = 1L),
      makeLogicalLearnerParam(id = "D", default = FALSE, requires = quote(!U)),
      makeIntegerLearnerParam(id = "W", default = weka.classifiers.trees.DecisionStump),
      makeLogicalLearnerParam(id = "D", default = FALSE),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "adaboostM1 Decision Trees",
    short.name = "adaboostM1",
    note = "NAs are directly passed to WEKA with `na.action = na.pass`.",
    callees = c("adaboostM1", "Weka_control")
  )
}

#' @export
trainLearner.classif.adaboostM1 = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = RWeka::Weka_control(..., Q = as.integer(runif(1, min = -.Machine$integer.max, max = .Machine$integer.max)))
  f = getTaskFormula(.task)
  RWeka::adaboostM1(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.adaboostM1 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
