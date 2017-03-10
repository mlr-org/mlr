
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
      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "write.forest", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
    properties = c("twoclass", "multiclass", "prob", "numerics", "factors", "ordered", "featimp", "weights", "oobpreds"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`), `verbose` output is disabled, `respect.unordered.factors` is set to `TRUE`. All settings are changeable.",
    callees = "ranger"
  )
}

#' @export
trainLearner.classif.ranger = function(.learner, .task, .subset, .weights = NULL, ...) {
  tn = getTaskTargetNames(.task)
  ranger::ranger(formula = NULL, dependent.variable = tn, data = getTaskData(.task, .subset),
    probability = (.learner$predict.type == "prob"), case.weights = .weights, ...)
}

#' @export
predictLearner.classif.ranger = function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata, ...)
  return(p$predictions)
}

#' @export
getOOBPredsLearner.classif.ranger = function(.learner, .model) {
  .model$learner.model$predictions
}

#' @export
getFeatureImportanceLearner.classif.ranger = function(.learner, .model, ...) {
  has.fiv = .learner$par.vals$importance
  if (is.null(has.fiv) || has.fiv == "none") {
    stop("You must set the learners parameter value for importance to
      'impurity' or 'permutation' to compute feature importance")
  }
  mod = getLearnerModel(.model)
  ranger::importance(mod)
}
