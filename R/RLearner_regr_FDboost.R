#' @export
makeRLearner.regr.FDboost = function() {
  makeRLearnerRegr(
    cl = "regr.FDboost",
    package = c("FDboost", "mboost"),
    par.set = fdboost.regr.ps,
    properties = c("numerics", "functionals"),
    name = "Functional linear array regression boosting",
    short.name = "FDboost",
    note = "Only allow one base learner for functional covariate and one base learner for scalar covariate, the parameters for these base learners are the same. Also we currently do not support interaction between scalar covariates"
  )
}

#' @export
trainLearner.regr.FDboost = function(.learner, .task, .subset, .weights = NULL, mstop = 100L,
  knots = 10L, df = 4L, bsignal.check.ident = FALSE, degree = 3L, differences = 1L,
  nu = 0.1, family = "Gaussian", custom.family.definition = NULL, nuirange = c(0, 100), d = NULL, ...) {

  family = switch(family,  # no binomial family
    Gaussian = mboost::Gaussian(),
    Laplace = mboost::Laplace(),
    Huber = mboost::Huber(d),
    Poisson = mboost::Poisson(),
    GammaReg = mboost::GammaReg(nuirange = nuirange),
    NBinomial = mboost::NBinomial(nuirange = nuirange),
    Hurdle = mboost::Hurdle(nuirange = nuirange),
    custom.family = custom.family.definition
  )
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu)
  hh = getFDboostFormulaMat(.task, knots = knots, df = df, bsignal.check.ident = bsignal.check.ident, degree = degree, differences = differences)
  FDboost::FDboost(formula = hh$form, timeformula = ~bols(1), data = hh$mat.list, control = ctrl, family = family)  # for scalar on function regression, the time formula is always ~bols(1)
}

#' @export
predictLearner.regr.FDboost = function(.learner, .model, .newdata, ...) {
  nl = as.list(.newdata)
  prd = predict(object = .model$learner.model, newdata = nl, which = NULL)
  prd
}
