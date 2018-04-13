# helper for fgam regression and classification
getFGAMFormulaMat = function(mdata, targetname, fns, Qtransform = TRUE, mgcv.s.k = -1L, mgcv.s.bs = "tp", mgcv.s.m = NA, mgcv.te_ti.m = NA, mgcv.te_ti.k = NA , basistype = "te", integration = "simpson", ...) {
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
    # also setup charvec of formula terms for func covars
    mat.list = namedList(fdns)
    #formula.terms = setNames(character(length = fdns))
    formula.terms = namedList(fdns)
    # for each functional covariate
    for (fdn in fdns) {
      # ... create a corresponding grid name
      gn = stri_paste(fdn, ".grid")
      # ... extract the corresponding original data into a list of matrices
      mat.list[[fdn]] = mdata[, fdn]
      # ... create a formula item
      # refund::af \int_{T}F(X_i(t),t)dt where refund::af means additive formula(FGAM), while refund::lf means linear Model (FLM)
      formula.terms[fdn] = switch(basistype,
        "s" = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, bs='%s', integration = '%s')", fdn, basistype, Qtransform, mgcv.s.k, mgcv.s.bs, integration),
        "te" = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, m= %s, integration = '%s')", fdn, basistype, Qtransform, mgcv.te_ti.k, mgcv.te_ti.m, integration))
    }
    # add grid names
    mat.list = c(mat.list, fdg)
  } else {
    stop("fgam does not support soley non-functional data")  # !hasFunctionalFeatures(m)
  }
  # add target names
  mat.list[[targetname]] = mdata[, targetname]
  # Create the formula and train the model
  form = as.formula(sprintf("%s~%s", targetname, collapse(unlist(formula.terms), "+")))
  return(list(form = form, mat.list = mat.list))
}

fgamParaSet = makeParamSet(
      makeDiscreteLearnerParam(id = "basistype", values = c("te", "s"), default = "te"),  # mgcv::te tensor(Kronecker) product smooths of X and T(mgcv::ti tensor product interaction), mgcv::s solely splines smooths to X
      makeIntegerVectorLearnerParam(id = "mgcv.s.k", default = c(-1L)),  # mgcv::s:k the dimension of the spline basis(#knots + 2) default: let mgcv choose
      makeDiscreteLearnerParam(id = "mgcv.s.bs", values = c("tp", "cr"), default = "tp"),  # mgcv::s:bs "tp"’ for thin plate regression spline, ‘"cr"’ for cubic regression spline
      makeIntegerVectorLearnerParam(id = "mgcv.s.m", lower = 1L, default = NA, special.vals = list(NA)),  # mgcv::s:m The order of the penalty for this term, default: let mgcv choose. The original default is NA but mlr will generate warnings for this.
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.m", lower = 1L, default = NA, special.vals = list(NA)),  # The order of the spline and its penalty (for smooth classes that use this) for each term.
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.k", lower = 1L, default = NA, special.vals = list(NA)),  # the dimension(s) of the bases used to represent the smooth term.  If not supplied then set to ‘5^d’.
      # skipped: argvals(indices of evaluation of ‘X’)
      makeDiscreteLearnerParam(id = "integration", values = c("simpson", "trapezoidal", "riemann"), default = "simpson"),
      # makeDiscreteLearnerParam(id = "presmooth", values = c("fpca.sc", "fpca.face", "fpca.ssvd", "fpca.bspline", "fpca.interpolate", NULL), default = NULL, special.vals = list(NULL)), # FIXME: currently not used in train
      # FIXME: skipped args: presmooth.opts, Xrange
      makeLogicalLearnerParam(id = "Qtransform", default = TRUE)  # c.d.f transform
    )

