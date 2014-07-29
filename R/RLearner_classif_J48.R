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
      makeLogicalLearnerParam(id = "J")
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "classif.J48",
    short.name = "J48",
    note = "Note that NAs are directly passed to WEKA with \\code{na.action = na.pass}"
  )
}

#' @export
trainLearner.classif.J48 = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ctrl = Weka_control(..., Q = as.integer(runif(1, min = -.Machine$integer.max, max = .Machine$integer.max)))
  f = getTaskFormulaAsString(.task)
  J48(as.formula(f), data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.J48 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
