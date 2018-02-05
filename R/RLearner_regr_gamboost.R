#' @export
makeRLearner.regr.gamboost = function() {
  makeRLearnerRegr(
    cl = "regr.gamboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "baselearner", values = c("bbs", "bols", "btree")),
      makeIntegerLearnerParam(id = "dfbase", default = 4L),
      makeNumericLearnerParam(id = "offset"),
      makeDiscreteLearnerParam(id = "family", default = "Gaussian", values = c("Gaussian", "Laplace",
        "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle", "custom.family")),
      # families 'Poisson', 'NBinomial' and 'Hurdle' are for count data
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0, 100),
        requires = quote(family %in% c("GammaReg", "NBinomial", "Hurdle"))),
      makeNumericLearnerParam(id = "d", requires = quote(family == "Huber")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      # 'risk' and 'stopintern' will be kept for completeness sake
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(),
    properties = c("numerics", "factors", "weights"),
    name = "Gradient Boosting with Smooth Components",
    short.name = "gamboost",
    callees = c("gamboost", "mboost_fit", "boost_control", "Gaussian", "Laplace",
      "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle")
  )
}

#' @export
# trainLearner.regr.gamboost = function(.learner, .task, .subset, .weights = NULL, family = "Gaussian", nuirange = c(0, 100), d = NULL, custom.family.definition, mstop, nu, risk, trace, stopintern, ...) {

trainLearner.regr.gamboost = function(.learner, .task, .subset, .weights = NULL, family = "Gaussian", nuirange = c(0, 100), d = NULL, custom.family.definition, mstop, nu, risk, trace, stopintern, ...) {
  requirePackages("mboost", why = "argument 'baselearner' requires package", suppress.warnings = TRUE)
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, trace, stopintern)
  family = convertFamilyParamRegr(family, family.nuirange, family.d)
  callLearnerWithOptionalWeights(mboost::gamboost, family = family, control = ctrl, ..., .task = task, .weights = .weights)
}

#' @export
predictLearner.regr.gamboost = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, ...)
  return(as.vector(p))
}
