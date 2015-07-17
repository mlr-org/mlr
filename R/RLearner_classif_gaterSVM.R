#' @export
makeRLearner.classif.gaterSVM = function() {
  makeRLearnerClassif(
    cl = "classif.gaterSVM",
    package = c("SwarmSVM", "e1071"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "m", lower = 1),
      makeNumericLearnerParam(id = "c", lower = 0),
      makeIntegerLearnerParam(id = "max.iter", lower = 1),
      makeIntegerLearnerParam(id = "hidden", default = 5, lower = 0),
      makeNumericLearnerParam(id = "learningrate", default = 0.01, lower = 0),
      makeNumericLearnerParam(id = "threshold", default = 0.01, lower = 0),
      makeIntegerLearnerParam(id = "stepmax", default = 100, lower = 1),
      makeNumericLearnerParam(id = "seed"),
      makeUntypedLearnerParam(id = "valid.x", default = NULL),
      makeUntypedLearnerParam(id = "valid.y", default = NULL),
      makeUntypedLearnerParam(id = "valid.metric", default = NULL),
      makeLogicalLearnerParam(id = "verbose", default = FALSE)
    ),
    properties = c("twoclass", "numerics"),
    name = "Mixture of SVMs with Neural Network Gater Function",
    short.name = "gaterSVM",
    note = ""
  )
}

#' @export
trainLearner.classif.gaterSVM = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  SwarmSVM::gaterSVM(x = d$data, y = d$target, ...)
}

#' @export
predictLearner.classif.gaterSVM = function(.learner, .model, .newdata, ...) {
  factor(predict(.model$learner.model, newdata = .newdata, ...),
    labels = .model$factor.levels$Class)
}
