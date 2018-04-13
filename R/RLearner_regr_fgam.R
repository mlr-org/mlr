# @title Regression of functional data by Functional Generalized Additive Models.
#
# @description
# FGAM estimate a smooth function for scalar on functional regression. Where the target is a scalar and the covarite is functional covariate, and the regression is an integration with respect to a link function upon the conditional expectation. $g\{E(Y_i|X_i)\} = \theta_0 +\int_{\tau} F\{X_i(\tau), \tau \}d{\tau}$, where the smooth function $F\{X(t), t\}$(estimation surface which is fitted againt all X(t), t pair) is not binded to be linear with the functional predictor $X(t)$ and takes an additive form which could quantify how important each functional point is to the response. The basic form for the smooth function is penalized splines. Typical application for FGAM is Diffusion tensor imaging(DTI) parallel diffusivity on Corpus Callosum where signal is higher for voxels where diffusion is hindered for multiple sclerosis patient PASAT score( A cognitive measure). For each pixel, there could be a tensor of 3*3 symmetric matrix which results from applying gradient from several non-collinear directions. In FGAM, X(t) is transformed to be in the interval of [0,1] by cdf tranform to let the limited observation data to fill all space of the surface and invariant to tranformation of functional predictors. FGAM support multiple functional covariate. Currently, only splines are used as base learner.
#

#' @export
makeRLearner.regr.fgam = function() {
  makeRLearnerRegr(
    cl = "regr.fgam",
    package = "refund",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "basistype", values = c("te", "s"), default = "te"),  # mgcv::te tensor(Kronecker) product smooths of X and T(mgcv::ti tensor product interaction), mgcv::s solely splines smooths to X
      makeIntegerVectorLearnerParam(id = "mgcv.s.k", default = c(-1L)),  # mgcv::s:k the dimension of the spline basis(#knots + 2) default: let mgcv choose
      makeDiscreteLearnerParam(id = "mgcv.s.bs", values = c("tp", "cr"), default = "tp"),  # mgcv::s:bs "tp"’ for thin plate regression spline, ‘"cr"’ for cubic regression spline
      makeIntegerVectorLearnerParam(id = "mgcv.s.m", lower = 1L, default = NA, special.vals = list(NA)),  # mgcv::s:m The order of the penalty for this term, default: let mgcv choose
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.m", lower = 1L, default = NA, special.vals = list(NA)),  # The order of the spline and its penalty (for smooth classes that use this) for each term.
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.k", lower = 1L, default = NA, special.vals = list(NA)),  # the dimension(s) of the bases used to represent the smooth term.  If not supplied then set to ‘5^d’.
      # skipped: argvals(indices of evaluation of ‘X’)
      makeDiscreteLearnerParam(id = "integration", values = c("simpson", "trapezoidal", "riemann"), default = "simpson"),
      # makeDiscreteLearnerParam(id = "presmooth", values = c("fpca.sc", "fpca.face", "fpca.ssvd", "fpca.bspline", "fpca.interpolate", NULL), default = NULL, special.vals = list(NULL)), # FIXME: currently not used in train
      # FIXME: skipped args: presmooth.opts, Xrange
      makeLogicalLearnerParam(id = "Qtransform", default = TRUE)  # c.d.f transform
    ),
    properties = c("numerics"),
    name = "functional general additive model",
    short.name = "FGAM"
  )
}

#' @export
trainLearner.regr.fgam = function(.learner, .task, .subset, .weights = NULL, Qtransform = TRUE, mgcv.s.k = c(-1L), mgcv.s.bs = "tp", mgcv.s.m = NA, mgcv.te_ti.m = NA, mgcv.te_ti.k = NA , basistype = "te", integration = "simpson", ...) {
  parlist = list(...)  #FIXME: currently this is not used, will be implemented in future version
  suppressMessages({d = getTaskData(.task, functionals.as = "dfcols")})
  m = getTaskData(.task, functionals.as = "matrix")
  tn = getTaskTargetNames(.task)
  fns = getTaskFeatureNames(.task)
  formmat = getFGAMFormulaMat(mdata = m, targetname = tn, fns = fns, d = d, Qtransform = Qtransform, mgcv.s.k = mgcv.s.k, mgcv.s.bs = mgcv.s.bs, mgcv.s.m = mgcv.s.m, mgcv.te_ti.m = mgcv.te_ti.m, mgcv.te_ti.k = mgcv.te_ti.k , basistype = basistype, integration = integration, ...)
  refund::pfr(formula = formmat$form, data = formmat$mat.list, family = gaussian())
}

#' @export
predictLearner.regr.fgam = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  nl = as.list(.newdata)
  pred = predict(.model$learner.model, newdata = nl, type = 'response')
  return(as.vector(pred))
}

