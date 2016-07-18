
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
      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "write.forest", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeDiscreteLearnerParam(id = "splitrule", values = c("logrank", "C"), default = "logrank")
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE, write.forest = TRUE),
    properties = c("numerics", "factors", "ordered", "rcens", "prob"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`), `verbose` output is disabled, `respect.unordered.factors` is set to `TRUE` and ranger's .forest object is kept for prediction (`keep.forest` = `TRUE`). All settings are changeable."
  )
}

#' @export
trainLearner.surv.ranger = function(.learner, .task, .subset, .weights, ...) {
  if (.learner$predict.type == "response")
  tn = getTaskTargetNames(.task)
  ranger::ranger(formula = NULL, dependent.variable.name = tn[1L],
    status.variable.name = tn[2L], data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.surv.ranger = function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata)
  rowMeans(p$chf)
}

#' @export
#' @rdname getFeatureImportanceLearner
getFeatureImportance.surv.ranger = function(.learner, .model, ...) {
  has.fiv = .learner$par.vals$importance
  if (is.null(has.fiv) || has.fiv == "none") {
    stop("You must set the learners parameter value for importance to
      'impurity' or 'permutation' to compute feature importance")
  }
  mod = getLearnerModel(.model)
  fiv = as.numeric(ranger::importance(mod))
  names(fiv) = .model$features
  return(fiv)
}
