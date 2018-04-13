# helper for fgam regression and classification
getFGAMFormulaMat = function(.task, Qtransform = TRUE, mgcv.s.k = -1L, mgcv.s.bs = "tp", mgcv.s.m = NA, mgcv.te_ti.m = NA, mgcv.te_ti.k = NA , basistype = "te", integration = "simpson", ...) {
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
        "s" = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, bs='%s', integration = '%s')", fdn, basistype, Qtransform, mgcv.s.k, mgcv.s.bs, integration),
        "te" = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, m= %s, integration = '%s')", fdn, basistype, Qtransform, mgcv.te_ti.k, mgcv.te_ti.m, integration))
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
  return(list(form = form, mat.list = mat.list))
}

