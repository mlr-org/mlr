#' @export
makeRLearner.classif.glmboost = function() {
  makeRLearnerClassif(
    cl = "classif.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      # FIXME: add family PropOdds, when mlr supports ordered factors as targets
      makeDiscreteLearnerParam(id = "family", default = "Binomial",
        values = c("Binomial", "AdaExp", "AUC", "custom.family")),
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      # makeNumericVectorLearnerParam(id = "nuirange", default = c(-0.5,-1), requires = quote(family == "PropOdds")),
      # makeNumericVectorLearnerParam(id = "offrange", default = c(-5,5), requires = quote(family == "PropOdds")),
      makeDiscreteLearnerParam(id = "Binomial.link", default = "logit", values = c("logit", "probit")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      # 'risk' and 'stopintern' will be kept for completeness sake
      makeLogicalLearnerParam(id = "center", default = TRUE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(family = "Binomial"),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Boosting for GLMs",
    short.name = "glmboost",
    note = "`family` has been set to `Binomial` by default. For 'family' 'AUC' and 'AdaExp' probabilities cannot be predcited.",
    callees = c("glmboost", "mboost_fit", "boost_control", "Binomial", "AdaExp", "AUC")
  )
}

#' @export
trainLearner.classif.glmboost = function(.learner, .task, .subset, .weights = NULL, Binomial.link = "logit", custom.family.definition, mstop, nu, risk, stopintern, trace, family, ...) {
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
    model = mboost::glmboost(f, data = d, control = ctrl, family = family, ...)
  } else {
    model = mboost::glmboost(f, data = d, control = ctrl, weights = .weights, family = family, ...)
  }
  model
}

#' @export
predictLearner.classif.glmboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if (.learner$predict.type == "prob") {
    if (!is.matrix(p) && is.na(p)) {
      stopf("The selected family %s does not support probabilities", getHyperPars(.learner)$family)
    } else {
      td = .model$task.desc
      # one observation prediction + family PropOdds returns a numeric vector instead of matrix
      # FIXME: add/change the outcommented line below to enable predicting one obs
      # (caution: check whether the right class is assigned)
      # if (nrow(.newdata) == 1 && is.vector(p)) dim(p) = c(1,2)
      p = p[, 1L]
      levs = c(td$negative, td$positive)
      return(propVectorToMatrix(p, levs))
    }
  } else {
    return(p)
  }
}
