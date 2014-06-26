#' @export
makeRLearner.classif.JRip = function() {
  makeRLearnerClassif(
    cl = "classif.JRip",
    package = "RWeka",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "F", default = 3L, lower = 2L),
      makeNumericLearnerParam(id = "N", default = 2, lower = 0),
      makeIntegerLearnerParam(id = "O", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "E", default = FALSE),
      makeLogicalLearnerParam(id = "P", default = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob")
  )
}

#' @export
trainLearner.classif.JRip = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  ctrl = Weka_control(..., S = as.integer(runif(1, min = -.Machine$integer.max, max = .Machine$integer.max)))
  JRip(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.JRip = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
