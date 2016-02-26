#' @export
makeRLearner.regr.blackboost = function() {
  makeRLearnerRegr(
    cl = "regr.blackboost",
    package = c("mboost", "party"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = mboost::Gaussian(), values = list(Gaussian = mboost::Gaussian(), Huber = mboost::Huber(), Laplace = mboost::Laplace())),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", default = "inbag", values = c("inbag", "oobag", "none")),
      makeDiscreteLearnerParam(id = "teststat", default = "quad", values = c("quad", "max")),
      makeDiscreteLearnerParam(id = "testtype", default = "Bonferroni", values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id = "mincriterion", default = 0.95, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", default = 7L, lower = 1L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", default = 9999L, lower = 1L, requires = quote(testtype=="MonteCarlo")),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 0L, lower = 0L),
      makeIntegerLearnerParam(id = "mtry", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxdepth", default = 0L, lower = 0L)
    ),
    properties = c("numerics", "factors", "weights", "missings"),
    name = "Gradient Boosting with Regression Trees",
    short.name = "blackbst",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness."
  )
}

trainLearner.regr.blackboost = function(.learner, .task, .subset, .weights = NULL, mstop, nu, risk, teststat, testtype, mincriterion, maxdepth, stump, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk)
  tc = learnerArgsToControl(party::ctree_control, teststat, testtype, mincriterion, maxdepth, stump)
  f = getTaskFormula(.task)
  if (!is.null(.weights))
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc, weights = .weights)
  else
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc)
}

predictLearner.regr.blackboost = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
