#' @export
makeRLearner.classif.PART = function() {
  makeRLearnerClassif(
    cl = "classif.PART",
    package = "RWeka",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "C", default = 0.25, lower = 0),
      makeIntegerLearnerParam(id = "M", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "R"),
      makeIntegerLearnerParam(id = "N", default = 3L, lower = 2L),
      makeLogicalLearnerParam(id = "B"),
      makeLogicalLearnerParam(id = "U"),
      makeLogicalLearnerParam(id = "J"),
      makeIntegerLearnerParam(id = "Q", tunable = FALSE),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "PART Decision Lists",
    short.name = "part",
    note = "NAs are directly passed to WEKA with `na.action = na.pass`."
  )
}

#' @export
trainLearner.classif.PART = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  ctrl = RWeka::Weka_control(..., Q = as.integer(runif(1L, min = -.Machine$integer.max, max = .Machine$integer.max)))
  RWeka::PART(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.PART = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
