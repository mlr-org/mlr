
#' @export
makeRLearner.regr.ranger = function() {
  makeRLearnerRegr(
    cl = "regr.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, default = 5L),
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
      makeDiscreteLearnerParam(id = "splitrule", values = c("variance", "extratrees", "maxstat"), default = "variance"),
      makeIntegerLearnerParam(id = "num.random.splits", lower = 1, default = 1, requires = quote(splitrule == "extratrees")),
      makeNumericLearnerParam(id = "alpha", lower = 0L, upper = 1L, default = 0.5, requires = quote(splitrule == "maxstat")),
      makeNumericLearnerParam(id = "minprop", lower = 0L, upper = 1L, default = 0.1, requires = quote(splitrule == "maxstat")),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "se.method", default = "jackknife", values = c("jackknife",  "sd"), requires = quote(se.method %in% "jackknife" && keep.inbag == TRUE), when = "both")
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
    properties = c("numerics", "factors", "ordered", "se", "oobpreds", "featimp"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`), `verbose` output is disabled, `respect.unordered.factors` is set to `TRUE`. All settings are changeable. Se estimation is mc bias-corrected jackknife after bootstrap, see '?regr.randomForest' for more details.",
    callees = "ranger"
  )
}

#' @export
trainLearner.regr.ranger = function(.learner, .task, .subset, .weights, se.method = "jackknife", keep.inbag = NULL, ...) {
  tn = getTaskTargetNames(.task)
  if (is.null(keep.inbag)) keep.inbag = (se.method == "jackknife" && .learner$predict.type == "se")
  ranger::ranger(formula = NULL, dependent.variable = tn, data = getTaskData(.task, .subset), keep.inbag = keep.inbag, ...)
}

#' @export
predictLearner.regr.ranger = function(.learner, .model, .newdata, se.method = "jackknife", ...) {

  predict.se = .learner$predict.type == "se"
  pred = predict(object = .model$learner.model, data = .newdata, predict.all = predict.se, ...)

  # Computes the mc bias-corrected jackknife after bootstrap
  if (predict.se) {
    p = rowMeans(pred$predictions)
    if (se.method == "jackknife") {
      se = jacknifeStandardError(
        aggregated.predictions = p,
        individual.predictions = pred$predictions,
        bag.counts = matrix(unlist(.model$learner.model$inbag.counts), ncol = length(.model$learner.model$inbag.counts), byrow = FALSE))
    } else if (se.method == "sd") {
      se = sdStandardError(
        individual.predictions = pred$predictions
        )
    }
    return(cbind(p, se))
  } else {
    return(pred$predictions)
  }
}

#' @export
getOOBPredsLearner.regr.ranger = function(.learner, .model) {
  .model$learner.model$predictions
}

#' @export
getFeatureImportanceLearner.regr.ranger = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.ranger(.learner, .model, ...)
}
