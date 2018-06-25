#' @export
makeRLearner.regr.blackboost = function() {
  makeRLearnerRegr(
    cl = "regr.blackboost",
    package = c("mboost", "party"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Gaussian", values = c("Gaussian", "Laplace",
        "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle", "custom.family")),
      # families 'Poisson', 'NBinomial' and 'Hurdle' are for count data
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0, 100), requires = quote(family %in% c("GammaReg", "NBinomial", "Hurdle"))),
      makeNumericLearnerParam(id = "d", requires = quote(family == "Huber")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", default = "inbag", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      # 'risk' and 'stopintern' will be kept for completeness sake
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "teststat", default = "max", values = c("quad", "max")),
      makeDiscreteLearnerParam(id = "testtype", default = "Teststatistic", values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id = "mincriterion", default = 0, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", default = 7L, lower = 1L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", default = 9999L, lower = 1L, requires = quote(testtype == "MonteCarlo")),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 0L, lower = 0L),
      makeIntegerLearnerParam(id = "mtry", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxdepth", default = 2L, lower = 0L)
    ),
    properties = c("numerics", "factors", "weights", "missings"),
    name = "Gradient Boosting with Regression Trees",
    short.name = "blackboost",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness.",
    callees = c("blackboost", "mboost_fit", "boost_control", "ctree_control",
      "Gaussian", "Laplace", "Huber", "GammaReg", "NBinomial", "Hurdle")
  )
}

trainLearner.regr.blackboost = function(.learner, .task, .subset, .weights = NULL, family = "Gaussian", nuirange = c(0, 100), d = NULL, custom.family.definition, mstop, nu, risk, stopintern, trace, teststat, testtype, mincriterion, maxdepth, savesplitstats, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, stopintern, trace)
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(maxdepth)) maxdepth = defaults$maxdepth
  if (missing(savesplitstats)) savesplitstats = defaults$savesplitstats
  tc =  learnerArgsToControl(party::ctree_control, teststat, testtype, mincriterion,
    maxdepth, savesplitstats, ...)
  family = switch(family,
    Gaussian = mboost::Gaussian(),
    Laplace = mboost::Laplace(),
    Huber = mboost::Huber(d),
    Poisson = mboost::Poisson(),
    GammaReg = mboost::GammaReg(nuirange = nuirange),
    NBinomial = mboost::NBinomial(nuirange = nuirange),
    Hurdle = mboost::Hurdle(nuirange = nuirange),
    custom.family = custom.family.definition
  )
  f = getTaskFormula(.task)
  if (!is.null(.weights))
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc, weights = .weights, family = family, ...)
  else
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc, family = family, ...)
}

predictLearner.regr.blackboost = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
