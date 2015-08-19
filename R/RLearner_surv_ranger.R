
#' @export
makeRLearner.surv.ranger = function() {
  makeRLearnerSurv(
    cl = "surv.ranger",
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
      makeIntegerLearnerParam(id = "seed", when = "both"),
      makeDiscreteLearnerParam(id = "splitrule", values = c("logrank", "C"), 
                               default = "logrank")
    ),
    properties = c("numerics", "factors", "rcens", "prob"),
    name = "Random Forests",
    short.name = "ranger",
    note = ""
  )
}

#' @export
trainLearner.surv.ranger = function(.learner, .task, .subset, .weigts, ...) {
  args = list(...)
  ranger::ranger(formula = getTaskFormula(.task), data = getTaskData(.task, .subset), 
         write.forest = TRUE, ...)
}

#' @export
predictLearner.surv.ranger = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "prob") {
    p = predict(object = .model$learner.model, data = .newdata, ...)
    return(p$survival)
  } else {
    stop("Unknown predict type")
  }
}