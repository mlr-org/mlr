# @title classification of functional data by Functional Generalized Additive Models.
#
# @description
# FGAM estimate a smooth function for scalar on functional regression. Where the target is a scalar and the covarite is functional covariate, and the regression is an integration with respect to a link function upon the conditional expectation. $g\{E(Y_i|X_i)\} = \theta_0 +\int_{\tau} F\{X_i(\tau), \tau \}d{\tau}$, where the smooth function $F\{X(t), t\}$(estimation surface which is fitted againt all X(t), t pair) is not binded to be linear with the functional predictor $X(t)$ and takes an additive form which could quantify how important each functional point is to the response. The basic form for the smooth function is penalized splines. Typical application for FGAM is Diffusion tensor imaging(DTI) parallel diffusivity on Corpus Callosum where signal is higher for voxels where diffusion is hindered for multiple sclerosis patient PASAT score( A cognitive measure). For each pixel, there could be a tensor of 3*3 symmetric matrix which results from applying gradient from several non-collinear directions. In FGAM, X(t) is transformed to be in the interval of [0,1] by cdf tranform to let the limited observation data to fill all space of the surface and invariant to tranformation of functional predictors. FGAM support multiple functional covariate. Currently, only splines are used as base learner.
#

#' @export
makeRLearner.classif.fgam = function() {
  makeRLearnerClassif(
    cl = "classif.fgam",
    package = "refund",
    par.set = fgam.ps,
    par.vals = fgam.par.vals,
    properties = c("functionals", "single.functional", "twoclass", "prob"),
    name = "functional general additive model",
    short.name = "FGAM"
  )
}

#' @export
trainLearner.classif.fgam = function(.learner, .task, .subset, .weights = NULL, ...) {
  parlist = list(...)
  tn = getTaskTargetNames(.task)
  fns = getTaskFeatureNames(.task)
  # tranform target to be 0 1
  vt = getTaskTargets(.task)
  uvt = unique(vt)
  dd = getTaskData(.task, target.extra = TRUE, functionals.as = "matrix")
  newtarget = sapply(dd$target, function(x) {
    if (x == uvt[1]) return(1); return(0)})
  nd = cbind(dd$data, newtarget)
  colnames(nd)[ncol(nd)] = tn
  formmat = getFGAMFormulaMat(mdata = nd, targetname = tn, fns = fns, parlist)
  formula = formmat$form
  data = formmat$mat.list
  mod = refund::pfr(formula = formula, data = data, family = binomial())
  mod$uvt = uvt
  mod
}

#' @export
predictLearner.classif.fgam = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  nl = as.list(.newdata)
  pred = predict(.model$learner.model, newdata = nl, type = "response")  # predict.fgam, predict.gam, predict.pfr
  if (.learner$predict.type  == "prob") {
    return(as.vector(pred))
  } else {
  uvt = .model$learner.model$uvt
  newpred = round(pred)
  newpred = sapply(newpred, function(x) {
    if (x == 1) return(uvt[1]); return(uvt[2])})
  newpred = as.factor(newpred)
  return(newpred)
  }
}

