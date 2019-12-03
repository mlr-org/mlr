#' @export
makeRLearner.regr.FDboost = function() {
  makeRLearnerRegr(
    cl = "regr.FDboost",
    package = c("FDboost", "mboost"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Gaussian", values = c("Gaussian", "Laplace",
        "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle", "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1), # the learning rate
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")), # list of parameters for the custom family
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0, 100), requires = quote(family %in% c("GammaReg", "NBinomial", "Hurdle"))), # distribution parameters for families
      makeNumericLearnerParam(id = "d", default = NULL, requires = quote(family == "Huber"), special.vals = list(NULL)), # delta parameter for Huber distribution
      makeNumericLearnerParam(id = "df", default = 4, lower = 0.5), # effective degrees of freedom, depend on the regularization parameter of the penality matrix and number of splines, must be the same for all base learners(covariates), the maximum value is the rank of the design matrix
      makeIntegerLearnerParam(id = "knots", default = 10L, lower = 1L), # determine the number of knots of splines, does not matter once there is sufficient number of knots, 30,40, 50 for example
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L), # degree of the b-spline
      makeIntegerLearnerParam(id = "differences", default = 1L, lower = 1L), # degree of the penalty
      makeLogicalLearnerParam(id = "bsignal.check.ident", default = FALSE, tunable = FALSE),
      forbidden = expression(df < differences)
    ),
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
  family = switch(family,
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
  FDboost::FDboost(formula = hh$form, timeformula = ~ bols(1), data = hh$mat.list, control = ctrl, family = family) # for scalar on function regression, the time formula is always ~bols(1)
}

#' @export
predictLearner.regr.FDboost = function(.learner, .model, .newdata, ...) {
  nl = as.list(.newdata)
  return(predict(object = .model$learner.model, newdata = nl, which = NULL))
}
