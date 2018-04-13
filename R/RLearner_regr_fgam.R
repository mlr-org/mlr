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
    par.set = fgamParaSet,
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
  formmat = getFGAMFormulaMat(mdata = m, targetname = tn, fns = fns, Qtransform = Qtransform, mgcv.s.k = mgcv.s.k, mgcv.s.bs = mgcv.s.bs, mgcv.s.m = mgcv.s.m, mgcv.te_ti.m = mgcv.te_ti.m, mgcv.te_ti.k = mgcv.te_ti.k , basistype = basistype, integration = integration, ...)
  refund::pfr(formula = formmat$form, data = formmat$mat.list, family = gaussian())
}

#' @export
predictLearner.regr.fgam = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  nl = as.list(.newdata)
  pred = predict(.model$learner.model, newdata = nl, type = 'response')  # predict.fgam, predict.gam, predict.pfr
  return(as.vector(pred))
}

