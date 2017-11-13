
#' @export
makeRLearner.classif.ranger = function() {
  makeRLearnerClassif(
    cl = "classif.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeNumericLearnerParam(id = "mtry.perc", lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeDiscreteLearnerParam("respect.unordered.factors", values = c("ignore", "order", "partition"), default = "ignore"),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "write.forest", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeDiscreteLearnerParam(id = "splitrule", values = c("gini", "extratrees"), default = "gini"),
      makeIntegerLearnerParam(id = "num.random.splits", lower = 1L, default = 1L, requires = quote(splitrule == "extratrees")),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = "order"),
    properties = c("twoclass", "multiclass", "prob", "numerics", "factors", "ordered", "featimp", "weights", "oobpreds"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`), `verbose` output is disabled, `respect.unordered.factors` is set to `order` for all splitrules. All settings are changeable. `mtry.perc` sets `mtry` to `mtry.perc*getTaskNFeats(.task)`. Default for `mtry` is the floor of square root of number of features in task. Default for `min.node.size` is 1 for classification and 10 for probability estimation.",
    callees = "ranger"
  )
}

#' @export
trainLearner.classif.ranger = function(.learner, .task, .subset, .weights = NULL, mtry, mtry.perc, min.node.size, ...) {
  tn = getTaskTargetNames(.task)
  if (missing(mtry)) {
    if (missing(mtry.perc)) {
      mtry = floor(sqrt(getTaskNFeats(.task)))
    } else {
      mtry = max(1, floor(mtry.perc * getTaskNFeats(.task)))
    }
  }
  if (missing(min.node.size)) {
    if (.learner$predict.type == "prob") {
      min.node.size = 10
    } else {
      min.node.size = 1
    }
  }
  ranger::ranger(formula = NULL, dependent.variable = tn, data = getTaskData(.task, .subset),
                 probability = (.learner$predict.type == "prob"), case.weights = .weights, mtry = mtry, min.node.size = min.node.size, ...)
}

#' @export
predictLearner.classif.ranger = function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata, ...)
  return(p$predictions)
}

#' @export
getOOBPredsLearner.classif.ranger = function(.learner, .model) {
  getLearnerModel(.model, more.unwrap = TRUE)$predictions
}

#' @export
getFeatureImportanceLearner.classif.ranger = function(.learner, .model, ...) {
  has.fiv = .learner$par.vals$importance
  if (is.null(has.fiv) || has.fiv == "none") {
    stop("You must set the learners parameter value for importance to
         'impurity' or 'permutation' to compute feature importance")
  }
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  ranger::importance(mod)
}
