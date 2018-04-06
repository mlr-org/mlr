#' @export
makeRLearner.classif.ctree = function() {
  makeRLearnerClassif(
    cl = "classif.ctree",
    package = "partykit",
    par.set = makeParamSet(
      # ctree
      makeFunctionLearnerParam(id = "na.action"),
      makeUntypedLearnerParam(id = "offset"),
      makeDiscreteVectorLearnerParam(id = "cluster"),
      makeFunctionLearnerParam(id = "ytrafo", default = NULL),
      makeFunctionLearnerParam(id = "converged", default = NULL),
      makeUntypedLearnerParam(id = "scores"),
      makeLogicalLearnerParam(id = "doFit", default = TRUE, tunable = FALSE),
      ## ctree_control
      makeDiscreteLearnerParam(id = "teststat", default = "quadratic",
                               values = c("quadratic", "maximum")),
      makeDiscreteLearnerParam(id = "splitstat", default = "quadratic",
                               values = c("quadratic", "maximum")),
      makeLogicalLearnerParam(id = "splittest", default = FALSE),
      makeDiscreteLearnerParam(id = "testtype", default = "Bonferroni",
                               values = c("Bonferroni", "MonteCarlo",
                                          "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id = "nmax", default = Inf, lower = 0, allow.inf = TRUE),
      makeNumericLearnerParam(id = "alpha", default = 0.05, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "mincriterion", lower = 0, upper = 1), # default = 1- alpha
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
      makeLogicalLearnerParam(id = "saveinfo", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "update", default = FALSE),
      makeDiscreteLearnerParam(id = "splitflavour", default = "ctree",
                               values = c("ctree", "exhaustive")) # exhaustive means it is not unbiased
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "ordered", "prob", "weights"),
    name = "Conditional Inference Trees",
    short.name = "ctree",
    note = "See also `?ctree_control`.",
    callees = c("ctree", "ctree_control")
  )
}



#' @export
trainLearner.classif.ctree = function(.learner, .task, .subset, .weights,
  logmincriterion, minsplit, minbucket, minprob, stump, lookahead, MIA,
  nresample, tol, maxsurrogate, numsurrogate, mtry, maxdepth, multiway,
  splittry, intersplit, majority, caseweights, applyfun, cores, saveinfo,
  update, splitflavour, ...) {

  ctrl = learnerArgsToControl(partykit::ctree_control,
    logmincriterion, minsplit, minbucket, minprob, stump, lookahead, MIA,
    nresample, tol, maxsurrogate, numsurrogate, mtry, maxdepth, multiway,
    splittry, intersplit, majority, caseweights, applyfun, cores, saveinfo,
    update, splitflavour)
  f = getTaskFormula(.task)
  partykit::ctree(f, data = getTaskData(.task, .subset), control = ctrl,
    weights = .weights, ...)
}

#' @export
predictLearner.classif.ctree = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    p = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, type = "prob", ...)
  }
  return(p)
}
