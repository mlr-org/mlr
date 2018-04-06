#' @export
makeRLearner.regr.fgam = function() {
  makeRLearnerRegr(
    cl = "regr.fgam",
    package = "refund",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "mgcv.s.k", default = c(-1L)),
      makeDiscreteLearnerParam(id = "mgcv.s.bs", values = c("tp", "cr"), default = "tp"),
      makeIntegerVectorLearnerParam(id = "mgcv.s.m", lower = 1L, default = NA, special.vals = list(NA)),
      makeIntegerVectorLearnerParam(id = "mgcv.teti.m", lower = 1L),  # see mgcv::te() documentation
      makeIntegerVectorLearnerParam(id = "mgcv.teti.k", lower = 1L),  # see mgcv::te() documentation
      # skipped argvals
      makeDiscreteLearnerParam(id = "basistype", values = c("te", "t2", "s"), default = "te"),
      makeDiscreteLearnerParam(id = "integration", values = c("simpson", "trapezoidal", "riemann"), default = "simpson"),
      makeDiscreteLearnerParam(id = "presmooth", values = c("fpca.sc", "fpca.face", "fpca.ssvd", "fpca.bspline",
        "fpca.interpolate", NULL), default = NULL, special.vals = list(NULL)),
      # skipped presmooth.opts, Xrange
      makeLogicalLearnerParam(id = "Qtransform", default = TRUE)  # c.d.f transform
    ),
    properties = c("functionals"),
    name = "functional general additive model",
    short.name = "FGAM",
    note = "Skipped parameters presmooth.opts, Xrange for now"
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
trainLearner.regr.fgam = function(.learner, .task, .subset, .weights = NULL,
  Qtransform = TRUE, mgcv.s.k = -1L, bs = "tp", ...) {

  df = getTaskData(.task, subset = .subset, target.extra = TRUE,
    functionals.as = "matrix")

  # Set up fdg = functional feature grids
  fdns = colnames(getFunctionalFeatures(df))
  # later on, the grid elements in mat.list should have suffix ".grid"
  fdg = namedList(fdns)
  fd.grids = lapply(fdns, function(name) seq_len(ncol(m[, name])))
  names(fd.grids) = fdns
  fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))



  # later on, the grid elements in mat.list should have suffix ".grid"
  names(fdg) = paste0(names(fdg), ".grid")
  fdns = names(fdf)
  # setup mat.list: for each func covar we add its data matrix and its grid
  # and once the target col also setup charvec of formula terms for func covars
  mat.list = namedList(fdns)
  formula.terms = namedList(fdns)
  for (fdn in fdns) {
    gn = paste0(fdn, ".grid")
    mat.list[[fdn]]=  as.matrix(d[, tdesc$fd.features[[fdn]], drop = FALSE])
    formula.terms[fdn] = sprintf("af(%s, basistype = 's', Qtransform = %d, k = %s, bs = %s)",     fdn, Qtransform, deparse(mgcv.s.k), bs)
  }
  mat.list = c(mat.list, fdg)
  mat.list[[tn]] = d[, tn]
  form = as.formula(sprintf("%s ~ %s", getTaskTargetNames(.task),
    collapse(formula.terms, "+")))
  refund::pfr(formula = form, data = mat.list)
}



reformat2list4mat2 = function(.data, tdesc){
  df =  .data
  fd.features = tdesc$fd.features
  fd.grids = tdesc$fd.grids
  tn = tdesc$target
  channel.list = tdesc$fd.features
  index.list = tdesc$fd.grids
  name4channel = names(index.list)
  num4channel = length(index.list)
  list4mat = list()
  for(i in 1:num4channel){
    list4mat[[name4channel[[i]]]]=  as.matrix(subset(df, select = channel.list[[i]]))
  }
  return(list4mat)
}

#' @export
predictLearner.regr.fgam = function(.learner, .model, .newdata, ...) {
  mextra_para  = list(...)
  tdesc = getTaskDesc(.model)
  list4mat = reformat2list4mat2(.newdata, tdesc)
  pred = predict(.model$learner.model, newdata = list4mat, type = 'response')
  return(as.vector(pred))
}
