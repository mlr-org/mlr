#' @export
makeRLearner.surv.cforest = function() {
  makeRLearnerSurv(
    cl = "surv.cforest",
    package = c("partykit", "survival"),
    par.set = makeParamSet(
      ## cforest
      makeUntypedLearnerParam(id = "offset"),
      makeDiscreteLearnerParam(id = "cluster"),
      makeDiscreteLearnerParam(id = "strata"),
      makeFunctionLearnerParam(id = "na.action"),
      makeFunctionLearnerParam(id = "ytrafo", default = NULL),
      makeUntypedLearnerParam(id = "scores"),
      makeIntegerLearnerParam(id = "ntree", lower = 1L, default = 500L),
      # Not sure if this is best practice (!!! Please check !!!):
      makeUntypedLearnerParam(id = "perturb", 
                              default = list(replace = FALSE, fraction = 0.632)),
      makeIntegerLearnerParam(id = "mtry", lower = 1L, 
                              default = ceiling(sqrt(nvar))), # is this possible?
      makeFunctionLearnerParam("applyfun", default = NULL, 
                               special.vals = list(NULL)),
      makeIntegerLearnerParam(id = "cores", default = NULL, lower = 1L, 
                              tunable = FALSE, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      
      ## ctree_control
      makeDiscreteLearnerParam(id = "teststat", default = "quadratic", 
                               values = c("quadratic", "maximum")),
      makeDiscreteLearnerParam(id = "splitstat", default = "quadratic", 
                               values = c("quadratic", "maximum")),
      makeLogicalLearnerParam(id = "splittest", default = FALSE),
      makeDiscreteLearnerParam(id = "testtype", default = "Univariate", 
                               values = c("Bonferroni", "MonteCarlo", 
                                          "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id = "nmax", default = Inf, lower = 0, allow.inf = TRUE),
      makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "mincriterion", default = 0, lower = 0, upper = 1), # default = 1- alpha
      makeNumericLearnerParam(id = "logmincriterion", lower = 0, upper = 1), # default = log(mincriterion)
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", default = 7L, lower = 1L),
      makeNumericLearnerParam(id = "minprob", default = 0.01, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeLogicalLearnerParam(id = "lookahead", default = FALSE),
      makeLogicalLearnerParam(id = "MIA", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", default = 9999L, lower = 1L, 
                              requires = quote(testtype=="MonteCarlo")),
      makeNumericLearnerParam(id = "tol", default = sqrt(.Machine$double.eps), 
                              lower = 0, upper = Inf), 
      makeIntegerLearnerParam(id = "maxsurrogate", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "numsurrogate", default = FALSE), 
      makeIntegerLearnerParam(id = "mtry", default = Inf, lower = 1, 
                              special.vals = list(Inf)),
      makeIntegerLearnerParam(id = "maxdepth", default = Inf, lower = 0L, 
                              special.vals = list(Inf)),
      makeLogicalLearnerParam(id = "multiway", default = FALSE),
      makeIntegerLearnerParam(id = "splittry", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "intersplit", default = FALSE),
      makeLogicalLearnerParam(id = "majority", default = FALSE),
      makeLogicalLearnerParam(id = "caseweights", default = TRUE),
      makeFunctionLearnerParam("applyfun", default = NULL, special.vals = list(NULL)),
      makeIntegerLearnerParam(id = "cores", default = NULL, lower = 1L, 
                              tunable = FALSE, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "saveinfo", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "update", default = FALSE),
      makeDiscreteLearnerParam(id = "splitflavour", default = "ctree", 
                               values = c("ctree", "exhaustive")) 
    ),
    properties = c("factors", "numerics", "ordered", "weights", "missings", "featimp"),
    par.vals = list(),
    name = "Random Forest based on Conditional Inference Trees",
    short.name = "crf",
    note = "See also `?ctree_control`.",
    callees = c("cforest", "ctree_control")
  )
}

#' @export
trainLearner.surv.cforest = function(.learner, .task, .subset, 
                                     logmincriterion, minsplit, minbucket, minprob, stump, lookahead, MIA,
                                     nresample, tol, maxsurrogate, numsurrogate, mtry, maxdepth, multiway,
                                     splittry, intersplit, majority, caseweights, applyfun, cores, saveinfo
                                     update, splitflavour, ...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(saveinfo)) safeinfo = defaults$saveinfo
  ctrl = learnerArgsToControl(partykit::ctree_control, 
                              logmincriterion, minsplit, minbucket, minprob, stump, lookahead, MIA,
                              nresample, tol, maxsurrogate, numsurrogate, mtry, maxdepth, multiway,
                              splittry, intersplit, majority, caseweights, applyfun, cores, saveinfo,
                              update, splitflavour)
  partykit::cforest(f, data = d, control = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.surv.cforest = function(.learner, .model, .newdata, ...) {
  # cforest returns median survival times; multiply by -1 so that high values correspond to high risk
  -1 * predict(.model$learner.model, newdata = .newdata, type = "response", ...)
}

#' @export
getFeatureImportanceLearner.surv.cforest = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.cforest(.learner, .model, ...)
}
