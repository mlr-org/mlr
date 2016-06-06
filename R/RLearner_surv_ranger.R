
#' @export
makeRLearner.surv.ranger = function() {
  makeRLearnerSurv(
    cl = "surv.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, default = 3L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeDiscreteLearnerParam(id = "splitrule", values = c("logrank", "C"), default = "logrank")
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE),
    properties = c("numerics", "factors", "rcens", "prob"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`) and `verbose` output is disabled. Both settings are changeable."
  )
}

#' @export
trainLearner.surv.ranger = function(.learner, .task, .subset, .weights, ...) {
  if (.learner$predict.type != "response")
    stop("Unsupported predict type")
  tn = getTaskTargetNames(.task)
  ranger::ranger(formula = NULL, dependent.variable.name = tn[1L], status.variable.name = tn[2L], data = getTaskData(.task, .subset),
                 write.forest = TRUE, ...)
}

#' @export
predictLearner.surv.ranger = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type != "response")
    stop("Unsupported predict type")
  p = predict(object = .model$learner.model, data = .newdata)
  rowMeans(p$chf)
}
