#' @export
makeRLearner.classif.blackboost = function() {
  makeRLearnerClassif(
    cl = "classif.blackboost",
    package = c("mboost", "party"),
    par.set = makeParamSet(
      # FIXME: add family PropOdds, when mlr supports ordered factors as targets
      makeDiscreteLearnerParam(id = "family", default = "Binomial",
        values = c("Binomial", "AdaExp", "AUC", "custom.family")),
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      #makeNumericVectorLearnerParam(id = "nuirange", default = c(-0.5,-1), requires = quote(family == "PropOdds")),
      #makeNumericVectorLearnerParam(id = "offrange", default = c(-5,5), requires = quote(family == "PropOdds")),
      makeDiscreteLearnerParam(id = "Binomial.link", default = "logit", values = c("logit", "probit")),
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
    par.vals = list(family = "Binomial"),
    properties = c("twoclass", "missings", "numerics", "factors", "prob", "weights"),
    name = "Gradient Boosting With Regression Trees",
    short.name = "blackboost",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness. `family` has been set to `Binomial` by default. For 'family' 'AUC' and 'AdaExp' probabilities cannot be predcited.",
    callees = c("blackboost", "mboost_fit", "boost_control", "ctree_control", "Binomial", "AdaExp", "AUC", "predict.mboost")
  )
}

#' @export
trainLearner.classif.blackboost = function(.learner, .task, .subset, .weights = NULL, Binomial.link = "logit", family, custom.family.definition, mstop, nu, risk, stopintern, trace, teststat, testtype, mincriterion, maxdepth, savesplitstats, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, stopintern, trace)
  # learner defaults need to be passed to ctree_control since tree_controls defaults
  # of blackboost differ from party::ctree_control defaults
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(maxdepth)) maxdepth = defaults$maxdepth
  if (missing(savesplitstats)) savesplitstats = defaults$savesplitstats
  tc =  learnerArgsToControl(party::ctree_control, teststat, testtype, mincriterion,
    maxdepth, savesplitstats, ...)
  f = getTaskFormula(.task)
  family = switch(family,
    Binomial = mboost::Binomial(link = Binomial.link),
    AdaExp = mboost::AdaExp(),
    AUC = mboost::AUC(),
    #PropOdds = mboost::PropOdds(nuirange = nuirange, offrange = offrange),
    custom.family = custom.family.definition)
  if (!is.null(.weights))
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc, weights = .weights, family = family, ...)
  else
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc, family = family, ...)
}

#' @export
predictLearner.classif.blackboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (.learner$predict.type == "prob") {
    if (!is.matrix(p) && is.na(p)){
      stopf("The selected family %s does not support probabilities", getHyperPars(.learner)$family)
    } else {
      td = .model$task.desc
      # one observation prediction + family PropOdds returns a numeric vector instead of matrix
      # FIXME: add/change the outcommented line below to enable predicting one obs
      # (caution: check whether the right class is assigned)
      # if (nrow(.newdata) == 1 && is.vector(p)) dim(p) = c(1,2)
      p = p[, 1L]
      levs = td$class.levels
      return(propVectorToMatrix(p, levs))
    }
  } else {
    return(p)
  }
}
