#' @title Regression of functional data by Functional Generalized Additive Models.
#'
#' @description
#' FGAM estimate a smooth function for scalar on functional regression. Where the target is a scalar and the covarite is functional covariate, and the regression is an integration with respect to a link function upon the conditional expectation. $g\{E(Y_i|X_i)\} = \theta_0 +\int_{\tau} F\{X_i(\tau), \tau \}d{\tau}$, where the smooth function $F\{X(t), t\}$(estimation surface which is fitted againt all X(t), t pair) is not binded to be linear with the functional predictor $X(t)$ and takes an additive form which could quantify how important each functional point is to the response. The basic form for the smooth function is penalized splines. Typical application for FGAM is Diffusion tensor imaging(DTI) where signal is higher for voxels where diffusion is hindered for multiple sclerosis patient. For each pixel, there could be a tensor of 3*3 matrix which results from applying gradient from several non-collinear directions. In FGAM, X(t) is transformed to be in the interval of [0,1] by cdf tranform to let the limited observation data to fill all space of the surface and invariant to tranformation of functional predictors. FGAM support multiple functional covariate.
#'

#' @export
makeRLearner.regr.fdafgam = function() {
  makeRLearnerRegr(
    cl = "regr.fdafgam",
    package = "refund",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "mgcv.s.k", default = c(-1L)),
      makeDiscreteLearnerParam(id = "mgcv.s.bs", values = c("tp", "cr"), default = "tp"),
      makeIntegerVectorLearnerParam(id = "mgcv.s.m", lower = 1L, default = 1L, special.vals = list(NA)),  # the default is actually NA not 1
      makeIntegerVectorLearnerParam(id = "mgcv.teti.m", lower = 1L),  # see mgcv::te() documentation
      makeIntegerVectorLearnerParam(id = "mgcv.teti.k", lower = 1L),  # see mgcv::te() documentation
      # skipped argvals
      makeDiscreteLearnerParam(id = "basistype", values = c("te", "t2", "s"), default = "te"),
      makeDiscreteLearnerParam(id = "integration", values = c("simpson", "trapezoidal", "riemann"), default = "simpson"),
      makeDiscreteLearnerParam(id = "presmooth", values = c("fpca.sc", "fpca.face", "fpca.ssvd", "fpca.bspline", "fpca.interpolate", NULL), default = NULL, special.vals = list(NULL)),
      # skipped presmooth.opts, Xrange
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
trainLearner.regr.fdafgam = function(.learner, .task, .subset, .weights = NULL, Qtransform = TRUE, mgcv.s.k = -1L, bs = "tp", ...) {

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
      formula.terms[fdn] = sprintf("af(%s, basistype = 's', Qtransform = %d, k=%s, bs=%s)", fdn, Qtransform, deparse(mgcv.s.k), bs)
    }
    # add grid names
    mat.list = c(mat.list, fdg)
  } else {
    fdns = NULL
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
