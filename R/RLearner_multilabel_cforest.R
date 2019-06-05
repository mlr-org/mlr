#' @export
makeRLearner.multilabel.cforest = function() {
  makeRLearnerMultilabel(
    cl = "multilabel.cforest",
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
    properties = c("numerics", "factors", "ordered", "missings", "weights", "prob"),
    par.vals = list(),
    name = "Random forest based on conditional inference trees",
    short.name = "cforest",
    callees = c("cforest", "cforest_control", "ctree_control")
  )
}

#' @export
trainLearner.multilabel.cforest = function(.learner, .task, .subset, .weights = NULL,
  ntree, mtry, replace, fraction, trace, teststat, testtype, mincriterion,
  minsplit, minbucket, stump, nresample, maxsurrogate,
  maxdepth, savesplitstats, ...) {

  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task)
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(replace)) replace = defaults$replace
  if (missing(fraction)) fraction = defaults$fraction
  ctrl = learnerArgsToControl(party::cforest_control, ntree, mtry, replace, fraction,
    trace, teststat, testtype, mincriterion, minsplit, minbucket, stump,
    nresample, maxsurrogate, maxdepth, savesplitstats)
  party::cforest(f, data = d, controls = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.multilabel.cforest = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, type = "prob", ...)
  p = do.call(rbind, p)
  if (.learner$predict.type == "response") {
    p = p >= 0.5
  } else {
    p
  }
}
