# checked props
#' @export
makeRLearner.classif.J48 = function() {
  makeRLearnerClassif(
    cl = "classif.J48",
    package = "RWeka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "U"),
      makeLogicalLearnerParam(id = "O"),
      makeNumericLearnerParam(id = "C", default = 0.25, lower = 0),
      makeIntegerLearnerParam(id = "M", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "R"),
      makeIntegerLearnerParam(id = "N", default = 3L, lower = 2L),
      makeLogicalLearnerParam(id = "B"),
      makeLogicalLearnerParam(id = "S"),
      makeLogicalLearnerParam(id = "L"),
      makeLogicalLearnerParam(id = "A"),
      makeLogicalLearnerParam(id = "J"),
      makeIntegerLearnerParam(id = "Q", tunable = FALSE),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "J48 Decision Trees",
    short.name = "j48",
    note = "NAs are directly passed to WEKA with `na.action = na.pass`."
  )
}

#' @export
trainLearner.classif.J48 = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = RWeka::Weka_control(..., Q = as.integer(runif(1, min = -.Machine$integer.max, max = .Machine$integer.max)))
  f = getTaskFormula(.task)
  RWeka::J48(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.J48 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
