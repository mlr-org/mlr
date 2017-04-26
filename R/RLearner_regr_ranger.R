
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
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
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
trainLearner.regr.ranger = function(.learner, .task, .subset, .weights, ...) {
  tn = getTaskTargetNames(.task)
  ranger::ranger(formula = NULL, dependent.variable = tn, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.ranger = function(.learner, .model, .newdata, ...) {

  p = predict(object = .model$learner.model, data = .newdata, ...)$predictions

  # Computes the mc bias-corrected jackknife after bootstrap
  if (.learner$predict.type == "se") {
    model = .model$learner.model
    model$inbag.counts = do.call(cbind, model$inbag.counts)
    model$inbag.counts = model$inbag.counts[rowSums(model$inbag.counts == 0) > 0, , drop = FALSE]
    n = nrow(model$inbag.counts)
    ntree = model$num.trees
    pred = predict(model, data = .newdata, predict.all = TRUE, ...)
    oob = model$inbag.counts == 0
    jack.n = apply(oob, 1, function(x) rowMeans(pred$predictions[, x, drop = FALSE]))
    if (is.vector(jack.n)) {
      jack.n = t(as.matrix(jack.n))
    }
    jack = (n - 1) / n * rowSums((jack.n - rowMeans(pred$predictions))^2)
    bias = (exp(1) - 1) * n / ntree^2 * rowSums((pred$predictions - rowMeans(pred$predictions))^2)
    jab = pmax(jack - bias, 0)
    se = sqrt(jab)
    return(cbind(p, se))
  } else {
    return(p)
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
