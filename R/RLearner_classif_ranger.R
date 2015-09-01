
#' @export
makeRLearner.classif.ranger = function() {
  makeRLearnerClassif(
    cl = "classif.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars)) 
      makeIntegerLearnerParam(id = "mtry", lower = 1L), 
      # FIXME: Add default value when data dependent defaults are implemented: min.node.size = 1 for classification, 10 for probability prediction 
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L), 
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), 
        default = "none"),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE,
        requires = expression(importance == "permutation")),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "prob", "numerics", "factors"),
    name = "Random Forests",
    short.name = "ranger",
    note = ""
  )
}

#' @export
trainLearner.classif.ranger <- function(.learner, .task, .subset, .weigts, ...) {
  args = list(...)
  ranger::ranger(formula = getTaskFormula(.task), data = getTaskData(.task, .subset), 
    write.forest = TRUE, probability = (.learner$predict.type == "prob"), ...)
}

#' @export
predictLearner.classif.ranger <- function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata, ...)
  return(p$predictions)
}