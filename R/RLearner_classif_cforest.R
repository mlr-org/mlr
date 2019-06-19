#' @export
makeRLearner.classif.cforest = function() {
  makeRLearnerClassif(
    cl = "classif.cforest",
    package = "party",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", lower = 1L, default = 500L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L, default = 5L),
      makeLogicalLearnerParam(id = "replace", default = FALSE),
      makeNumericLearnerParam(id = "fraction", lower = 0, upper = 1, default = 0.632,
        requires = quote(replace == FALSE)),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "teststat", values = c("quad", "max"), default = "quad"),
      makeDiscreteLearnerParam(id = "testtype",
        values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
        default = "Univariate"),
      makeNumericLearnerParam(id = "mincriterion", lower = 0, default = 0),
      makeIntegerLearnerParam(id = "minsplit", lower = 1L, default = 20L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L, default = 7L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", lower = 1L, default = 9999L),
      makeIntegerLearnerParam(id = "maxsurrogate", lower = 0L, default = 0L),
      makeIntegerLearnerParam(id = "maxdepth", lower = 0L, default = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "prob", "factors", "numerics", "ordered", "weights", "missings", "featimp"),
    par.vals = list(),
    name = "Random forest based on conditional inference trees",
    short.name = "cforest",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness.",
    callees = c("cforest", "cforest_control", "ctree_control")
  )
}

#' @export
trainLearner.classif.cforest = function(.learner, .task, .subset,
  .weights = NULL, ntree, mtry, replace, fraction, trace, teststat,
  testtype, mincriterion, minsplit, minbucket, stump,
  nresample, maxsurrogate, maxdepth, savesplitstats, ...) {

  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)

  # default handling necessary because the default of controls is `cforest_unbiased()` which does not allow all parameters (e.g. replace)
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(replace)) replace = defaults$replace
  if (missing(fraction)) fraction = defaults$fraction
  ctrl = learnerArgsToControl(party::cforest_control, ntree, mtry, replace,
    fraction, trace, teststat, testtype, mincriterion,
    minsplit, minbucket, stump, nresample, maxsurrogate,
    maxdepth, savesplitstats)

  party::cforest(f, data = d, controls = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.classif.cforest = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "prob") {
    p = predict(.model$learner.model, newdata = .newdata, type = "prob", ...)
    # FIXME: this will break for nrow(.newdata) == 1? do not use sapply!
    p = t(sapply(p, "["))
    colnames(p) = .model$task.desc$class.levels
  } else {
    p = predict(.model$learner.model, newdata = .newdata, ...)
  }
  p
}

#' @export
getFeatureImportanceLearner.classif.cforest = function(.learner, .model, auc = FALSE, ...) {
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  if (auc) {
    party::varimpAUC(mod, ...)
  } else {
    party::varimp(mod, ...)
  }
}
