#' @export
makeRLearner.regr.ctree = function() {
  makeRLearnerRegr(
    cl = "regr.ctree",
    package = "party",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "teststat", default = "quad", values = c("quad", "max")),
      makeDiscreteLearnerParam(id = "testtype", default = "Bonferroni", values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id = "mincriterion", default = 0.95, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", default = 7L, lower = 1L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", default = 9999L, lower = 1L, requires = quote(testtype == "MonteCarlo")),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 0L, lower = 0L),
      makeIntegerLearnerParam(id = "mtry", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxdepth", default = 0L, lower = 0L)
    ),
    properties = c("missings", "numerics", "factors", "ordered", "weights"),
    name = "Conditional Inference Trees",
    short.name = "ctree",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness.",
    callees = c("ctree", "ctree_control")
  )
}

#' @export
trainLearner.regr.ctree = function(.learner, .task, .subset, .weights = NULL, teststat, testtype,
  mincriterion, minsplit, minbucket, stump, nresample, maxsurrogate, mtry,
  savesplitstats, maxdepth, ...) {
  ctrl = learnerArgsToControl(party::ctree_control, teststat, testtype, mincriterion, minsplit,
    minbucket, stump, nresample, maxsurrogate, mtry, savesplitstats, maxdepth)
  f = getTaskFormula(.task)
  party::ctree(f, data = getTaskData(.task, .subset), controls = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.regr.ctree = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
