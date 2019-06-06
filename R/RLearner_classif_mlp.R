#' @export
makeRLearner.classif.mlp = function() {
  makeRLearnerClassif(
    cl = "classif.mlp",
    package = "RSNNS",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "size", default = 5, lower = 1),
      makeIntegerLearnerParam(id = "maxit", default = 100, lower = 100),
      makeUntypedLearnerParam(id = "initFunc", default = "Randomize_Weights"),
      makeNumericVectorLearnerParam(id = "initFuncParams"),
      makeUntypedLearnerParam(id = "learnFunc", default = "Std_Backpropagation"),
      makeNumericVectorLearnerParam(id = "learnFuncParams"),
      makeUntypedLearnerParam(id = "updateFunc", default = "Topological_Order"),
      makeNumericVectorLearnerParam(id = "updateFuncParams"),
      makeUntypedLearnerParam(id = "hiddenActFunc", default = "Act_Logistic"),
      makeLogicalLearnerParam(id = "shufflePatterns", default = TRUE),
      makeLogicalLearnerParam(id = "linOut", default = FALSE),
      makeUntypedLearnerParam(id = "inputsTest"),
      makeUntypedLearnerParam(id = "targetsTest"),
      makeUntypedLearnerParam(id = "pruneFunc"),
      makeUntypedLearnerParam(id = "pruneFuncParams")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Multi-Layer Perceptron",
    short.name = "mlp",
    callees = "mlp"
  )
}

#' @export
trainLearner.classif.mlp = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  onehot = RSNNS::decodeClassLabels(d$target)
  RSNNS::mlp(x = d$data, y = onehot, ...)
}

#' @export
predictLearner.classif.mlp = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, response = "class", prob = "raw")
  pred = predict(.model$learner.model, .newdata)
  colnames(pred) = .model$factor.levels[[1]]

  if (type == "class") {
    classes = colnames(pred)[max.col(pred)]
    return(as.factor(classes))
  }
  return(pred)
}
