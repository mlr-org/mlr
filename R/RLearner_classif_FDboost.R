#' @export
makeRLearner.classif.FDboost = function() {
  makeRLearnerClassif(
    cl = "classif.FDboost",
    package = c("FDboost", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Binomial", values = c("AdaExp", "Binomial", "AUC", "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1), # the learning rate
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")), # list of parameters for the custom family
      makeDiscreteLearnerParam(id = "Binomial.link", default = "logit",
        values = c("logit", "probit"), requires = quote(family == "Binomial")),
      makeNumericLearnerParam(id = "df", default = 4, lower = 0.5), # effective degrees of freedom, depend on the regularization parameter of the penality matrix and number of splines, must be the same for all base learners(covariates), the maximum value is the rank of the design matrix
      makeIntegerLearnerParam(id = "knots", default = 10L, lower = 1L), # determine the number of knots of splines, does not matter once there is sufficient number of knots, 30,40, 50 for example
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L), # degree of the b-spline
      makeIntegerLearnerParam(id = "differences", default = 1L, lower = 1L), # degree of the penalty
      makeLogicalLearnerParam(id = "bsignal.check.ident", default = FALSE, tunable = FALSE), # identifiability check by testing matrix degeneracy
      forbidden = expression(df < differences)
    ),
    par.vals = list(family = "Binomial"),
    properties = c("functionals", "numerics", "twoclass", "prob"),
    name = "Functional linear array classification boosting",
    short.name = "FDboost",
    note = "Uses only one base learner per functional or scalar covariate.
      Uses the same hyperparameters for every baselearner.
      Currently does not support interaction between scalar covariates.
      Default for family has been set to 'Binomial', as 'Gaussian' is not applicable."
  )
}

#' @export
trainLearner.classif.FDboost = function(.learner, .task, .subset, .weights = NULL,
  mstop = 100L, knots = 10L, df = 4L, bsignal.check.ident = FALSE, degree = 3L, differences = 1L,
  Binomial.link = "logit", nu = 0.1, family = "Binomial", custom.family.definition = NULL,
  d = NULL, ...) {

  family = switch(family,
    Binomial = mboost::Binomial(link = Binomial.link),
    AdaExp = mboost::AdaExp(),
    AUC = mboost::AUC(),
    custom.family = custom.family.definition)

  # Note: Family PropOdds applicable only for ordinal classification:
  # PropOdds = mboost::PropOdds(nuirange = nuirange, offrange = offrange)

  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu)

  hh = getFDboostFormulaMat(.task, knots = knots, df = df, bsignal.check.ident = bsignal.check.ident,
    degree = degree, differences = differences)
  if (.learner$predict.type == "prob") {
    td = getTaskDesc(.task)
    levs = c(td$negative, td$positive)
    hh$mat.list[[getTaskTargetNames(.task)]] = factor(hh$mat.list[[getTaskTargetNames(.task)]], levs)
  }

  FDboost::FDboost(formula = hh$form, timeformula = ~ bols(1), data = hh$mat.list, control = ctrl, family = family)
}

#' @export
predictLearner.classif.FDboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response") # additional parameters passed to mboost::predict()

  p = predict(.model$learner.model, newdata = as.list(.newdata), type = type, ...)

  if (.learner$predict.type == "prob") {
    if (!is.matrix(p) && any(is.na(p))) {
      stopf("The selected family %s does not support probabilities", getHyperPars(.learner)$family)
    } else {
      td = .model$task.desc
      if (!is.null(dim(p))) {
        p = p[, 1L]
      }
      return(propVectorToMatrix(p, c(td$negative, td$positive)))
    }
  } else {
    return(p)
  }
}
