#' @export
makeRLearner.classif.gamboost = function() {
  makeRLearnerClassif(
    cl = "classif.gamboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "baselearner", values = c("bbs", "bols", "btree")),
      makeIntegerLearnerParam(id = "dfbase", default = 4L),
      makeNumericLearnerParam(id = "offset"),
      # FIXME: add family PropOdds, when mlr supports ordered factors as targets
      makeDiscreteLearnerParam(id = "family", default = "Binomial",
        values = c("AdaExp", "Binomial", "AUC", "custom.family")),
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeDiscreteLearnerParam(id = "Binomial.link", default = "logit",
        values = c("logit", "probit"), requires = quote(family == "Binomial")),
      # makeNumericVectorLearnerParam(id = "nuirange", default = c(-0.5, -1), requires = quote(family == "PropOdds")),
      # makeNumericVectorLearnerParam(id = "offrange", default = c(-5,5), requires = quote(family == "PropOdds")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      # 'risk' and 'stopintern' will be kept for completeness sake
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(family = "Binomial"),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Gradient boosting with smooth components",
    short.name = "gamboost",
    note = "`family` has been set to `Binomial()` by default. For 'family' 'AUC' and 'AdaExp' probabilities cannot be predicted.",
    callees = c("gamboost", "mboost_fit", "boost_control", "Binomial", "AdaExp", "AUC")
  )
}

#' @export
trainLearner.classif.gamboost = function(.learner, .task, .subset, .weights = NULL, Binomial.link = "logit", mstop, nu, risk, stopintern, trace, family, custom.family.definition, ...) {
  requirePackages("mboost", why = "argument 'baselearner' requires package", suppress.warnings = TRUE)
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, stopintern, trace)
  family = switch(family,
    Binomial = mboost::Binomial(link = Binomial.link),
    AdaExp = mboost::AdaExp(),
    AUC = mboost::AUC(),
    # PropOdds = mboost::PropOdds(nuirange = nuirange, offrange = offrange),
    custom.family = custom.family.definition)
  d = getTaskData(.task, .subset)
  if (.learner$predict.type == "prob") {
    td = getTaskDesc(.task)
    levs = c(td$negative, td$positive)
    d[, getTaskTargetNames(.task)] = factor(d[, getTaskTargetNames(.task)], levs)
  }
  f = getTaskFormula(.task)
  if (is.null(.weights)) {
    model = mboost::gamboost(f, data = d, control = ctrl, family = family, ...)
  } else {
    model = mboost::gamboost(f, data = d, control = ctrl, weights = .weights, family = family, ...)
  }
  model
}

#' @export
predictLearner.classif.gamboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (.learner$predict.type == "prob") {
    if (!is.matrix(p) && is.na(p)) {
      stopf("The selected family %s does not support probabilities", getHyperPars(.learner)$family)
    } else {
      td = .model$task.desc
      p = p[, 1L]
      levs = c(td$negative, td$positive)
      return(propVectorToMatrix(p, levs))
    }
  } else {
    return(p)
  }
}
