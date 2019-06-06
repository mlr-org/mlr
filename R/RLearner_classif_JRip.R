#' @export
makeRLearner.classif.JRip = function() {
  makeRLearnerClassif(
    cl = "classif.JRip",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "F", default = 3L, lower = 2L),
      makeNumericLearnerParam(id = "N", default = 2, lower = 0),
      makeIntegerLearnerParam(id = "O", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "D", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "S", tunable = FALSE),
      makeLogicalLearnerParam(id = "E", default = FALSE),
      makeLogicalLearnerParam(id = "P", default = FALSE),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "Propositional Rule Learner",
    short.name = "jrip",
    note = "NAs are directly passed to WEKA with `na.action = na.pass`.",
    callees = c("JRip", "Weka_control")
  )
}

#' @export
trainLearner.classif.JRip = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  ctrl = RWeka::Weka_control(..., S = as.integer(runif(1, min = -.Machine$integer.max, max = .Machine$integer.max)))
  RWeka::JRip(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.JRip = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
