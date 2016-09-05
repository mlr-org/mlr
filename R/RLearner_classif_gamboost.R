#' @export
makeRLearner.classif.gamboost = function() {
  makeRLearnerClassif(
    cl = "classif.gamboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "baselearner", default = "bbs", values = c("bbs", "bols", "btree")),
      makeIntegerLearnerParam(id = "dfbase", default = 4),
      makeNumericLearnerParam(id = "offset"),
      makeDiscreteLearnerParam(id = "family", default = "Binomial", values = c("AdaExp", "Binomial", "AUC")),
      # FIXME default of glmboost() for family is Gaussian()
      makeDiscreteLearnerParam(id = "Binomial.link", default = "logit", values = c("logit", "probit"),
        requires = quote(family == "Binomial")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Gradient boosting with smooth components",
    short.name = "gambst",
    note = "`family` has been set to `Binomial()` by default."
  )
}

#' @export
trainLearner.classif.gamboost = function(.learner, .task, .subset, .weights = NULL, offset = NULL, mstop, nu, risk, stopintern, trace, family, Binomial.link, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, stopintern, trace)
  d = getTaskData(.task, .subset)
  if (.learner$predict.type == "prob") {
    td = getTaskDescription(.task)
    levs = c(td$negative, td$positive)
    d[, getTaskTargetNames(.task)] = factor(d[, getTaskTargetNames(.task)], levs)
  }
  f = getTaskFormula(.task)
  family = switch(family,
    Binomial = mboost::Binomial(link = Binomial.link),
    AdaExp = mboost::AdaExp(),
    AUC = mboost::AUC()
    )
  if (is.null(.weights)) {
    model = mboost::gamboost(f, data = d, control = ctrl, family = family, ...)
  } else  {
    model = mboost::gamboost(f, data = d, control = ctrl, weights = .weights, family = family, ...)
  }
  model
}

#' @export
predictLearner.classif.gamboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  fam = getLearnerParVals(.learner)$family
  if (.learner$predict.type  == "prob") {
    if (fam == "Binomial") {
      td = .model$task.desc
      p = p[, 1L]
      levs = c(td$negative, td$positive)
      return(propVectorToMatrix(p, levs))
    } else {
      stopf("Predict.type needs to be 'response' for family = '%s'.", fam)
    }
  } else {
    return(p)
  }
}