# helper for fgam regression and classification
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
    # also setup charvec of formula terms for func covars
    mat.list = namedList(fdns)
    formula.terms = namedList(fdns)
    # for each functional covariate
    for (fdn in fdns) {
      # ... create a corresponding grid name
      gn = stri_paste(fdn, ".grid")
      # ... extract the corresponding original data into a list of matrices
      mat.list[[fdn]] = mdata[, fdn]
      # ... create a formula item
      # refund::af \int_{T}F(X_i(t),t)dt where refund::af means additive formula(FGAM), while refund::lf means linear Model (FLM)
      formula.terms[fdn] = switch(parlist$basistype,
        "s" = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, bs='%s', integration = '%s')",
          fdn, parlist$basistype, parlist$Qtransform, parlist$mgcv.s.k, parlist$mgcv.s.bs, parlist$integration),
        "te" = sprintf("af(%s, basistype = '%s', Qtransform = %s, k=%s, m= %s, integration = '%s')",
          fdn, parlist$basistype, parlist$Qtransform, parlist$mgcv.te_ti.k, parlist$mgcv.te_ti.m, parlist$integration))
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


fgam.ps = makeParamSet(
      makeDiscreteLearnerParam(id = "basistype", values = c("te", "s"), default = "te"),  # mgcv::te tensor(Kronecker) product smooths of X and T(mgcv::ti tensor product interaction), mgcv::s solely splines smooths to X
      makeIntegerVectorLearnerParam(id = "mgcv.s.k", default = c(-1L)),  # mgcv::s:k the dimension of the spline basis(#knots + 2) default: let mgcv choose
      makeDiscreteLearnerParam(id = "mgcv.s.bs", values = c("tp", "cr"), default = "tp"),  # mgcv::s:bs "tp"’ for thin plate regression spline, ‘"cr"’ for cubic regression spline
      makeIntegerVectorLearnerParam(id = "mgcv.s.m", lower = 1L, special.vals = list(NA)),  # mgcv::s:m The order of the penalty for this term, default: let mgcv choose. The original default is NA but mlr will generate warnings for this.
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.m", lower = 1L, special.vals = list(NA)),  # The order of the spline and its penalty (for smooth classes that use this) for each term. The original default is NA but mlr will generate warnings for this.
      makeIntegerVectorLearnerParam(id = "mgcv.te_ti.k", lower = 1L, special.vals = list(NA)),  # the dimension(s) of the bases used to represent the smooth term.  If not supplied then set to ‘5^d’. The original default is NA but mlr will generate warnings for this.
      # skipped: argvals(indices of evaluation of ‘X’)
      makeDiscreteLearnerParam(id = "integration", values = c("simpson", "trapezoidal", "riemann"), default = "simpson"),
      # makeDiscreteLearnerParam(id = "presmooth", values = c("fpca.sc", "fpca.face", "fpca.ssvd", "fpca.bspline", "fpca.interpolate", NULL), default = NULL, special.vals = list(NULL)), # FIXME: currently not used in train
      # FIXME: skipped args: presmooth.opts, Xrange
      makeLogicalLearnerParam(id = "Qtransform", default = TRUE)  # c.d.f transform
    )

fgam.par.vals = list(basistype = "te", integration = "simpson", Qtransform = TRUE)

getBinomialTarget = function(.task)  {
  vt = getTaskTargets(.task)
  uvt = unique(vt)
  dd = getTaskData(.task, target.extra = TRUE, functionals.as = "matrix")
  newtarget = sapply(dd$target, function(x) {if (x == uvt[1]) return(1); return(0)})
  return(list(newtarget = newtarget, uvt = uvt))
}

FDboost.ps = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Gaussian", values = c("Gaussian", "Laplace",
        "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle", "custom.family")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),  # the learning rate
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),  # list of parameters for the custom family
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0, 100), requires = quote(family %in% c("GammaReg", "NBinomial", "Hurdle"))),  # distribution parameters for families
      makeNumericLearnerParam(id = "d", default = NULL, requires = quote(family == "Huber"), special.vals = list(NULL)), # delta parameter for Huber distribution
      # makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")), we don't need this in FDboost
      makeNumericLearnerParam(id = "df", default = 4, lower = 0.5),  # effective degrees of freedom, depend on the regularization parameter of the penality matrix and number of splines, must be the same for all base learners(covariates), the maximum value is the rank of the design matrix
      # makeDiscreteLearnerParam(id = "baselearner", values = c("bbs", "bols")),  # we don't use "btree" in FDboost
      makeIntegerLearnerParam(id = "knots", default = 10L, lower = 1L),  # determine the number of knots of splines, does not matter once there is sufficient number of knots, 30,40, 50 for example
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L),  # degree of the b-spline
      makeIntegerLearnerParam(id = "differences", default = 1L, lower = 1L),  # degree of the penalty
      makeLogicalLearnerParam(id = "bsignal.check.ident", default = FALSE, tunable = FALSE)  # identifiability check by testing matrix degeneracy
    )

getFDboostFormulaMat = function(.task, knots, df, bsignal.check.ident, degree, differences) {
suppressMessages({d = getTaskData(.task, functionals.as = "dfcols")})
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
    # also setup charvec of formula terms for func covars
    mat.list = namedList(fdns)
    #formula.terms = setNames(character(length = fdns))
    formula.terms = namedList(fdns)
    # for each functional covariate
    for (fdn in fdns) {
      # ... create a corresponding grid name
      gn = stri_paste(fdn, ".grid")
      # ... extract the corresponding original data into a list of matrices
      mat.list[[fdn]] = tdata[, fdn]
      # ... create a formula item
      formula.terms[fdn] = sprintf("bsignal(%s, %s, knots = %i, df = %f, degree = %i, differences = %i, check.ident = %s)",
        fdn, gn, knots, df, degree, differences, bsignal.check.ident)
    }
    # add grid names
    mat.list = c(mat.list, fdg)
  } else {
    fdns = NULL  # no functional features
  }

  # Add formula to each scalar covariate, if there is no scalar covariate, this fd.scalars will be empty
  for (fsn in setdiff(colnames(tdata), c(fdns, tn))) {
    mat.list[[fsn]] = as.vector(as.matrix(d[, fsn, drop = FALSE]))
    formula.terms[fsn] = sprintf("bbs(%s, knots = %i, df = %f, degree = %i, differences = %i)",
      fsn, knots, df, degree, differences)
  }


  # add target names
  mat.list[[tn]] = d[, tn]

  # Create the formula and train the model
  form = as.formula(sprintf("%s ~ %s", tn, collapse(unlist(formula.terms), "+")))
  return(list(mat.list = mat.list, form = form))
}

getClassifFamily = function(family, Binomial.link) {
  family = switch(family,
    Binomial = mboost::Binomial(link = Binomial.link),
    AdaExp = mboost::AdaExp(),
    AUC = mboost::AUC(),
    #PropOdds = mboost::PropOdds(nuirange = nuirange, offrange = offrange),
    custom.family = custom.family.definition)
  return(family)
}


#suppressMessages({d = getTaskData(.task, functionals.as = "dfcols")})
#  m = getTaskData(.task, functionals.as = "matrix")
#  tn = getTaskTargetNames(.task)
#
#  formula.terms = namedList()
#  mat.list = namedList(getTaskFeatureNames(.task))
#
#  # Treat functional covariates
#  if (hasFunctionalFeatures(m)) {
#    fdns = colnames(getFunctionalFeatures(m))
#    # later on, the grid elements in mat.list should have suffix ".grid"
#    fdg = namedList(fdns)
#    fd.grids = lapply(fdns, function(name) seq_len(ncol(m[, name])))
#    names(fd.grids) = fdns
#    fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))
#    # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
#    # also setup charvec of formula terms for func covars
#    mat.list = namedList(fdns)
#    #formula.terms = setNames(character(length = fdns))
#    formula.terms = namedList(fdns)
#    # for each functional covariate
#    for (fdn in fdns) {
#      # ... create a corresponding grid name
#      gn = stri_paste(fdn, ".grid")
#      # ... extract the corresponding original data into a list of matrices
#      mat.list[[fdn]] = m[, fdn]
#      # ... create a formula item
#      formula.terms[fdn] = sprintf("bsignal(%s, %s, knots = %i, df = %f, degree = %i, differences = %i, check.ident = %s)",
#        fdn, gn, knots, df, degree, differences, bsignal.check.ident)
#    }
#    # add grid names
#    mat.list = c(mat.list, fdg)
#  } else {
#    fdns = NULL
#  }
#
#  # Add formula to each scalar covariate, if there is no scalar covariate, this fd.scalars will be empty
#  for (fsn in setdiff(colnames(m), c(fdns, tn))) {
#    mat.list[[fsn]] = as.vector(as.matrix(d[, fsn, drop = FALSE]))
#    formula.terms[fsn] = sprintf("bbs(%s, knots = %i, df = %f, degree = %i, differences = %i)",
#      fsn, knots, df, degree, differences)
#  }
#
#
#  # add target names
#  mat.list[[tn]] = d[, tn]
#
#  # Create the formula and train the model
#  form = as.formula(sprintf("%s ~ %s", tn, collapse(unlist(formula.terms), "+")))
#
