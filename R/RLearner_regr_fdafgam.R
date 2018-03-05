# @title Regression of functional data by Functional Generalized Additive Models.
#
# @description
# FGAM estimate a smooth function for scalar on functional regression. Where the target is a scalar and the covarite is functional covariate, and the regression is an integration with respect to a link function upon the conditional expectation. $g\{E(Y_i|X_i)\} = \theta_0 +\int_{\tau} F\{X_i(\tau), \tau \}d{\tau}$, where the smooth function $F\{X(t), t\}$(estimation surface which is fitted againt all X(t), t pair) is not binded to be linear with the functional predictor $X(t)$ and takes an additive form which could quantify how important each functional point is to the response. The basic form for the smooth function is penalized splines. Typical application for FGAM is Diffusion tensor imaging(DTI) parallel diffusivity on Corpus Callosum where signal is higher for voxels where diffusion is hindered for multiple sclerosis patient PASAT score( A cognitive measure). For each pixel, there could be a tensor of 3*3 symmetric matrix which results from applying gradient from several non-collinear directions. In FGAM, X(t) is transformed to be in the interval of [0,1] by cdf tranform to let the limited observation data to fill all space of the surface and invariant to tranformation of functional predictors. FGAM support multiple functional covariate. Currently, only splines are used as base learner.
#

#' @export
makeRLearner.regr.fdafgam = function() {
  makeRLearnerRegr(
    cl = "regr.fdafgam",
    package = "refund",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "basistype", values = c("te", "s"), default = "te"),  # te:tensor product smooths, mgcv::s: Defining smooths in GAM formulae
      makeIntegerVectorLearnerParam(id = "mgcv.s.k", default = c(-1L)),  # mgcv::s the dimension of the basis used to represent the smooth term
      makeDiscreteLearnerParam(id = "mgcv.s.bs", values = c("tp", "cr"), default = "tp"),  # mgcv::s "tp"’ for thin plate regression spline, ‘"cr"’ for cubic regression spline
      makeIntegerVectorLearnerParam(id = "mgcv.s.m", lower = 1L, default = NA, special.vals = list(NA)),  # mgcv::s The order of the penalty for this term
      # mgcv::te, mgcv::tr Define tensor product smooths or tensor product interactions
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.m", lower = 1L, default = NA, special.vals = list(NA)),  # The order of the spline and its penalty (for smooth classes that use this) for each term.
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.k", lower = 1L, default = NA, special.vals = list(NA)),  # the dimension(s) of the bases used to represent the smooth term.  If not supplied then set to ‘5^d’.
      # skipped argvals
      makeDiscreteLearnerParam(id = "integration", values = c("simpson", "trapezoidal", "riemann"), default = "simpson"),
      makeDiscreteLearnerParam(id = "presmooth", values = c("fpca.sc", "fpca.face", "fpca.ssvd", "fpca.bspline", "fpca.interpolate", NULL), default = NULL, special.vals = list(NULL)), # FIXME: currently not used in train
      # skipped args: presmooth.opts, Xrange
      makeLogicalLearnerParam(id = "Qtransform", default = TRUE)  # c.d.f transform
    ),
    properties = c("numerics"),
    name = "functional general additive model",
    short.name = "FGAM"
  )
}

#  @param s.k [\code{integer}] \cr
#  the dimension of the basis used to represent the smooth term.
#  The default depends on the number of variables that the smooth is a function of.
#  it should be chosen to be large enough that you are reasonably sure of having
#  enough degrees of freedom to represent the underlying ‘truth’ reasonably well,
#  but small enough to maintain reasonable computational efficiency.
#  k must be chosen: the defaults are essentially arbitrary??????????
#  see mgcv::choose.k using mgcv::gam.check
#' @export
trainLearner.regr.fdafgam = function(.learner, .task, .subset, .weights = NULL, Qtransform = TRUE, mgcv.s.k = -1L, mgcv.s.bs = "tp", mgcv.te_ti.m = NA, mgcv.te_ti.k = NA , basistype = "s", integration = "simpson", ...) {
  parlist = list(...)  #FIXME: currently this is not used, will be implemented in future version
  suppressMessages({d = getTaskData(.task, functionals.as = "dfcols")})
  m = getTaskData(.task, functionals.as = "matrix")
  tn = getTaskTargetNames(.task)

  formula.terms = namedList()
  mat.list = namedList(getTaskFeatureNames(.task))

  # Treat functional covariates
  if (hasFunctionalFeatures(m)) {
    fdns = colnames(getFunctionalFeatures(m))
    # later on, the grid elements in mat.list should have suffix ".grid"
    fdg = namedList(fdns)
    fd.grids = lapply(fdns, function(name) seq_len(ncol(m[, name])))
    names(fd.grids) = fdns
    fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))
    # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
    # also setup charvec of formula terms for func covars
    mat.list = namedList(fdns)
    #formula.terms = setNames(character(length = fdns))
    formula.terms = namedList(fdns)
    # for each functional covariate
    for (fdn in fdns) {
      # ... create a corresponding grid name
      gn = stri_paste(fdn, ".grid")
      # ... extract the corresponding original data into a list of matrices
      mat.list[[fdn]] = m[, fdn]
      # ... create a formula item
      # refund::af \int_{T}F(X_i(t),t)dt where refund::af means additive formula(FGAM), while refund::lf means linear Model (FLM)
      formula.terms[fdn] = switch(basistype,
        "s" = sprintf("af(%s, basistype = '%s', Qtransform = %d, k=%s, bs='%s', integration = '%s')", fdn, basistype, Qtransform, mgcv.s.k, mgcv.s.bs, integration),
        "te" = sprintf("af(%s, basistype = '%s', Qtransform = %d, k=%s, m='%s', integration = '%s')", fdn, basistype, Qtransform, mgcv.te_ti.k, mgcv.te_ti.m, integration))
    }
    # add grid names
    mat.list = c(mat.list, fdg)
  } else {
    stop("fgam does not support soley non-functional data")  # !hasFunctionalFeatures(m)
  }
  # add target names
  mat.list[[tn]] = d[, tn]
  # Create the formula and train the model
  form = as.formula(sprintf("%s~%s", tn, collapse(unlist(formula.terms), "+")))
  refund::pfr(formula = form, data = mat.list)
}

#' @export
predictLearner.regr.fdafgam = function(.learner, .model, .newdata, ...) {
  assert(hasFunctionalFeatures(.newdata))
  nl = as.list(.newdata)
  pred = predict(.model$learner.model, newdata = nl, type = 'response')
  return(as.vector(pred))
}
