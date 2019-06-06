# checked props
#' @export
makeRLearner.classif.J48 = function() {
  makeRLearnerClassif(
    cl = "classif.J48",
    package = "RWeka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "U", default = FALSE),
      makeLogicalLearnerParam(id = "O", default = FALSE),
      makeNumericLearnerParam(id = "C", default = 0.25, lower = .Machine$double.eps, upper = 1 - .Machine$double.eps, requires = quote(!U && !R)),
      makeIntegerLearnerParam(id = "M", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "R", default = FALSE, requires = quote(!U)),
      makeIntegerLearnerParam(id = "N", default = 3L, lower = 2L, requires = quote(!U && R)),
      makeLogicalLearnerParam(id = "B", default = FALSE),
      makeLogicalLearnerParam(id = "S", default = FALSE, requires = quote(!U)),
      makeLogicalLearnerParam(id = "L", default = FALSE),
      makeLogicalLearnerParam(id = "A", default = FALSE),
      makeLogicalLearnerParam(id = "J", default = FALSE),
      makeIntegerLearnerParam(id = "Q", tunable = FALSE),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "J48 Decision Trees",
    short.name = "j48",
    note = "NAs are directly passed to WEKA with `na.action = na.pass`.",
    callees = c("J48", "Weka_control")
  )
}

#' @export
trainLearner.classif.J48 = function(.learner, .task, .subset, .weights = NULL, ...) {
  ctrl = RWeka::Weka_control(..., Q = as.integer(runif(1, min = -.Machine$integer.max, max = .Machine$integer.max)))
  f = getTaskFormula(.task)
  RWeka::J48(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.J48 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
