# helper for fgam regression and classification
# @title Regression of functional data by Functional Generalized Additive Models.
#
# @description
# FGAM estimates a smooth function for scalar on functional regression.
# Where the target is a scalar and the covarite is functional covariate,
# and the regression is an integration with respect to a link function upon the conditional
# expectation. $g\{E(Y_i|X_i)\} = \theta_0 +\int_{\tau} F\{X_i(\tau), \tau \}d{\tau}$,
# where the smooth function $F\{X(t), t\}$(estimation surface which is fitted againt
# all X(t), t pair) is not binded to be linear with the functional predictor $X(t)$ and
# takes an additive form which could quantify how important each functional point is to the
# response. The basic form for the smooth function is penalized splines. Typical application
# for FGAM is Diffusion tensor imaging(DTI) parallel diffusivity on Corpus
# Callosum where signal is higher for voxels where diffusion is hindered for
# multiple sclerosis patient PASAT score( A cognitive measure). For each pixel,
# there could be a tensor of 3*3 symmetric matrix which results from applying gradient
# from several non-collinear directions. In FGAM, X(t) is transformed to be in the interval of
# [0,1] by cdf tranform to let the limited observation data to fill all space of the surface
# and invariant to tranformation of functional predictors. FGAM support multiple functional
# covariate. Currently, only splines are used as base learner.

fgam.ps = makeParamSet(
  makeDiscreteLearnerParam(id = "basistype", values = c("te", "ti"), default = "te"),
  # mgcv::te tensor(Kronecker) product smooths of X and T(mgcv::ti tensor product interaction plus marginal effect(called main effect in mgcv package))
  makeIntegerVectorLearnerParam(id = "mgcv.te_ti.m", lower = 1L, special.vals = list(NA)),
  # The order of the spline and its penalty (for smooth classes that use this) for each term. The original default is NA but mlr will generate warnings for this.
  makeIntegerVectorLearnerParam(id = "mgcv.te_ti.k", lower = 1L, special.vals = list(NA)),
  # the dimension(s) of the bases used to represent the smooth term.  If not supplied then set to ?5^d?. The original default is NA but mlr will generate warnings for this.
  # skipped: argvals(indices of evaluation of ?X?)
  makeDiscreteLearnerParam(id = "integration", values = c("simpson", "trapezoidal", "riemann"), default = "simpson"),
  # makeDiscreteLearnerParam(id = "presmooth", values = c("fpca.sc", "fpca.face", "fpca.ssvd", "fpca.bspline", "fpca.interpolate", NULL), default = NULL, special.vals = list(NULL)),
  makeLogicalLearnerParam(id = "Qtransform", default = TRUE) # c.d.f transform
)

fgam.par.vals = list(basistype = "te", integration = "simpson", Qtransform = TRUE, mgcv.te_ti.m = NA, mgcv.te_ti.k = NA)

getFGAMFormulaMat = function(mdata, targetname, fns, parlist) {
  formula.terms = namedList()
  mat.list = namedList(fns)
  # Treat functional covariates
  if (hasFunctionalFeatures(mdata)) {
    fdns = colnames(getFunctionalFeatures(mdata))
    # later on, the grid elements in mat.list should have suffix ".grid"
    fdg = namedList(fdns)
    fd.grids = lapply(fdns, function(name) seq_len(ncol(mdata[, name])))
    names(fd.grids) = fdns
    fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))
    # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
    # also setup character vector of formula terms for functional covariates
    mat.list = namedList(fdns)
    formula.terms = namedList(fdns)
    # for each functional covariate
    for (fdn in fdns) {
      # ... create a corresponding grid name
      gn = stri_paste(fdn, ".grid")
      # ... extract the corresponding original data into a list of matrices
      mat.list[[fdn]] = mdata[, fdn]
      # ... create a formula item
      # refund::af \int_{T}F(X_i(t),t)dt where refund::af means additive formula(FGAM),
      # while refund::lf means linear Model (FLM)
      fkm = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, m= %s, integration = '%s')",
        fdn, parlist$basistype, parlist$Qtransform, parlist$mgcv.te_ti.k, parlist$mgcv.te_ti.m, parlist$integration)
      fk = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, integration = '%s')",
        fdn, parlist$basistype, parlist$Qtransform, parlist$mgcv.te_ti.k, parlist$integration)
      fm = sprintf("af(%s, basistype = '%s', Qtransform = %s, m=%s, integration = '%s')",
        fdn, parlist$basistype, parlist$Qtransform, parlist$mgcv.te_ti.m, parlist$integration)
      f0 = sprintf("af(%s, basistype = '%s', Qtransform = %s, integration = '%s')",
        fdn, parlist$basistype, parlist$Qtransform, parlist$integration)
      mf = fkm
      if (is.na(parlist$mgcv.te_ti.k)) mf = fm
      if (is.na(parlist$mgcv.te_ti.m)) mf = fk
      if (is.na(parlist$mgcv.te_ti.m) && is.na(parlist$mgcv.te_ti.k)) mf = f0
      formula.terms[fdn] = mf
    }
    # add grid names
    mat.list = c(mat.list, fdg)
  } else {
    stop("fgam does not support soley non-functional data")
  }
  # add target names
  mat.list[[targetname]] = mdata[, targetname]
  # Create the formula and train the model
  form = as.formula(sprintf("%s~%s", targetname, collapse(unlist(formula.terms), "+")))
  return(list(form = form, mat.list = mat.list))
}

getBinomialTarget = function(.task) {
  vt = getTaskTargets(.task)
  uvt = unique(vt)
  dd = getTaskData(.task, target.extra = TRUE, functionals.as = "matrix")
  newtarget = sapply(dd$target, function(x) {
    if (x == uvt[1]) {
      return(1)
    }
    return(0)
  })
  return(list(newtarget = newtarget, uvt = uvt))
}

getFDboostFormulaMat = function(.task, knots, df, bsignal.check.ident, degree, differences) {

  tdata = getTaskData(.task, functionals.as = "matrix")
  tn = getTaskTargetNames(.task)
  formula.terms = namedList()
  mat.list = namedList(getTaskFeatureNames(.task))
  # Treat functional covariates
  if (hasFunctionalFeatures(tdata)) {
    fdns = colnames(getFunctionalFeatures(tdata))
    # later on, the grid elements in mat.list should have suffix ".grid"
    fdg = namedList(fdns)
    fd.grids = lapply(fdns, function(name) seq_len(ncol(tdata[, name])))
    names(fd.grids) = fdns
    fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))
    # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
        # also setup character vector of formula terms for functional covariates
    mat.list = namedList(fdns)
    formula.terms = namedList(fdns)
    # for each functional covariate
    for (fdn in fdns) {
      # ... create a corresponding grid name
      gn = stri_paste(fdn, ".grid")
      # ... extract the corresponding original data into a list of matrices
      mat.list[[fdn]] = tdata[, fdn]
      # ... create a formula item
      formula.terms[fdn] = sprintf("bsignal(%s, %s, knots = %i, df = %f, degree = %i,
        differences = %i, check.ident = %s)",
        fdn, gn, knots, df, degree, differences, bsignal.check.ident)
    }
    # add grid names
    mat.list = c(mat.list, fdg)
  } else {
    fdns = NULL # no functional features
  }

  # Add formula to each scalar covariate, if there is no scalar covariate, this fd.scalars will be empty
  for (fsn in setdiff(colnames(tdata), c(fdns, tn))) {
    mat.list[[fsn]] = as.vector(as.matrix(tdata[, fsn, drop = FALSE]))
    formula.terms[fsn] = sprintf("bbs(%s, knots = %i, df = %f, degree = %i, differences = %i)",
      fsn, knots, df, degree, differences)
  }

  # add target names
  mat.list[[tn]] = tdata[, tn]

  # Create the formula and train the model
  form = as.formula(sprintf("%s ~ %s", tn, collapse(unlist(formula.terms), "+")))
  return(list(mat.list = mat.list, form = form))
}
