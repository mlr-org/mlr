#' @export
makeRLearner.classif.glmboost = function() {
  makeRLearnerClassif(
    cl = "classif.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Binomial",
        values = c("AdaExp", "Binomial", "PropOdds", "custom.family")),
      # FIXME default of glmboost() for family is Gaussian()
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeNumericVectorLearnerParam(id = "nuirange", default = c(-0.5,-1), requires = quote(family == "PropOdds")),
      makeNumericVectorLearnerParam(id = "offrange", default = c(-5,5), requires = quote(family == "PropOdds")),
      makeDiscreteLearnerParam(id = "Binomial.link", default = "logit", values = c("logit", "probit")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      makeLogicalLearnerParam(id = "center", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(family = "Binomial"),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Boosting for GLMs",
    short.name = "glmbst",
    note = "`family` has been set to `Binomial()` by default."
  )
}

#' @export
trainLearner.classif.glmboost = function(.learner, .task, .subset, .weights = NULL, mstop, nu, risk, stopintern, trace, family, custom.family.definition, nuirange = c(-0.5,-1), offrange = c(-5,5), Binomial.link = "logit", ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, stopintern, trace)
  family = switch(family,
    Binomial = mboost::Binomial(Binomial.link),
    AdaExp = mboost::AdaExp(),
    PropOdds = mboost::PropOdds(nuirange = nuirange, offrange = offrange),
    custom.family = custom.family.definition)
  d = getTaskData(.task, .subset)
  if (.learner$predict.type == "prob") {
    td = getTaskDescription(.task)
    levs = c(td$negative, td$positive)
    d[, getTaskTargetNames(.task)] = factor(d[, getTaskTargetNames(.task)], levs)
  }
  f = getTaskFormula(.task)
  if (is.null(.weights)) {
    model = mboost::glmboost(f, data = d, control = ctrl, family = family, ...)
  } else  {
    model = mboost::glmboost(f, data = d, control = ctrl, weights = .weights, family = family, ...)
  }
  model
}

#' @export
predictLearner.classif.glmboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  fam = getLearnerParVals(.learner)$family
  if (.learner$predict.type  == "prob") {
    if (fam == "AdaExp") {
      stopf("prediction.type = 'prob' not implemented for family %s", fam)
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