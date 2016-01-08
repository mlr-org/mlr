#' @export
makeRLearner.regr.cforest = function() {
  makeRLearnerRegr(
    cl = "regr.cforest",
    package = "party",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", lower = 1L, default = 500L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L, default = 5L),
      makeLogicalLearnerParam(id = "replace", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeNumericLearnerParam(id = "fraction", lower = 0, upper = 1, default = 0.632),
      makeDiscreteLearnerParam(id = "teststat", values = c("quad", "max"), default = 'quad'),
      makeLogicalLearnerParam(id = "pvalue", default = TRUE),
      makeDiscreteLearnerParam(id = "testtype",
        values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
        default = 'Univariate'),
      makeNumericLearnerParam(id = "mincriterion", lower = 0, default = 0),
      makeNumericLearnerParam(id = "minprob", lower = 0, default = 0.01),
      makeIntegerLearnerParam(id = "minsplit", lower = 1L, default = 20L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L, default = 7L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeLogicalLearnerParam(id = "randomsplits", default = TRUE),
      makeIntegerLearnerParam(id = "nresample", lower = 1L, default = 9999L),
      makeIntegerLearnerParam(id = "maxsurrogate", lower = 0L, default = 0L),
      makeIntegerLearnerParam(id = "maxdepth", lower = 0L, default = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "ordered", "weights", "missings"),
    par.vals = list(),
    name = "Random Forest Based on Conditional Inference Trees",
    short.name = "cforest",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness."
  )
}

#' @export
trainLearner.regr.cforest = function(.learner, .task, .subset, .weights = NULL,
                                     ntree, mtry, replace, fraction, trace, pvalue,
                                     teststat, testtype, mincriterion, minprob,
                                     minsplit, minbucket, stump, randomsplits,
                                     nresample, maxsurrogate, maxdepth,
                                     savesplitstats,...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(replace)) replace = defaults$replace
  if (missing(fraction)) fraction = defaults$fraction
  ctrl = learnerArgsToControl(party::cforest_control, ntree, mtry, replace, fraction,
                              trace, pvalue, teststat, testtype, mincriterion,
                              minprob, minsplit, minbucket, stump, randomsplits,
                              nresample, maxsurrogate, maxdepth, savesplitstats)
  party::cforest(f, data = d, controls = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.regr.cforest = function(.learner, .model, .newdata, ...) {
  as.vector(predict(.model$learner.model, newdata = .newdata, ...))
}
