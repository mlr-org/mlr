
#' @export
makeRLearner.regr.ranger = function() {
  makeRLearnerRegr(
    cl = "regr.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L), 
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L), 
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), 
        default = "none"),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE,
        requires = expression(importance == "permutation")),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both"),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both"),
      makeIntegerLearnerParam(id = "seed", when = "both")
    ),
    properties = c("numerics", "factors"),
    name = "Random Forests",
    short.name = "ranger",
    note = ""
  )
}

#' @export
trainLearner.regr.ranger = function(.learner, .task, .subset, .weigts, ...) {
  args = list(...)
  ranger::ranger(formula = getTaskFormula(.task), data = getTaskData(.task, .subset), 
    write.forest = TRUE, ...)
}

#' @export
predictLearner.regr.ranger = function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata, ...)
  return(p$predictions)
}