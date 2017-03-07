#' @title Penalized functional regression.
#'
#' @description
#' Learner for penalized functional regression from the pfr function of refund. 
#' FIXME: currently I only have one parameter, but actually this function has 
#' a million parameters and I need to find a way to specify them.
#' 
#' @export
makeRLearner.fdaregr.pfr = function() {
  makeRLearnerRegr(
    cl = "fdaregr.pfr",
    package = "refund",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "mgcv.s.k", default = -1L),  # see mgcv::s() documentation
      makeIntegerVectorLearnerParam(id = "mgcv.teti.m", lower = 1L),  # see mgcv::te() documentation
      makeLogicalLearnerParam(id = "Qtransform", default = TRUE)  # c.d.f transform
    ),
    properties = c("numerics"),
    name = "penalized functional regression",
    short.name = "pfr"
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
trainLearner.fdaregr.pfr = function(.learner, .task, .subset, .weights = NULL, Qtransform = TRUE, mgcv.s.k = -1L, ...) {
  d = getTaskData(.task, subset = .subset)
  tn = getTaskTargetNames(.task)
  tdesc = getTaskDescription(.task)
  fdf = tdesc$fd.features
  fdg = tdesc$fd.grids
  # later on, the grid elements in mat.list should have suffix ".grid"
  names(fdg) = paste0(names(fdg), ".grid")
  fdns = names(fdf)
  # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
  # also setup charvec of formula terms for func covars
  mat.list = namedList(fdns)
  formula.terms = namedList(fdns)
  for (fdn in fdns) {
    gn = paste0(fdn, ".grid")
    mat.list[[fdn]]=  as.matrix(d[, tdesc$fd.features[[fdn]], drop = FALSE])
    formula.terms[fdn] = genFDAFormula(fdn, Qtransform, mgcv.s.k)
  }
  mat.list = c(mat.list, fdg)
  mat.list[[tn]] = d[, tn]
  form = as.formula(sprintf("%s ~ %s", tn, collapse(formula.terms, "+")))
  refund::pfr(formula = form, data = mat.list)
}

# FIXME: this function should return all possible smooth functions/surfaces supported by 
# mgcv but 
# for simplicity we only implement the af(FGAM) with s function, but there are a lot left
genFDAFormula = function(fdn, Qtransform, mgcv.s.k ){
  sprintf("af(%s, basistype = 's', Qtransform = %d, k=%d)", fdn, Qtransform, mgcv.s.k)
  # arg.vals indices of evaluation of x 
  #sprintf("af(%s, basistype = 'te', Qtransform = TRUE, k=%d, bs =%s, m =%d)", fdn, mgcv.te.k, mgcv.te.bs, mgcv.te.m)
  #sprintf("af(%s, basistype = 'ti', Qtransform = TRUE, k=%d, bs =%s, m =%d)", fdn, mgcv.ti.k, mgcv.te.bs, mgcv.ti.m)
  #sprintf("fpc(%s)", fdn), re, lf, lf.vd()
  #sprintf("peer(%s, argvals = seq(0, 1, length = %d), integration = %s, pentype =%s)",fdn, peer.length, peer.integration, peer.pentype)
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
predictLearner.fdaregr.pfr = function(.learner, .model, .newdata, ...) {
  mextra_para  = list(...)
  tdesc = getTaskDescription(.model)
  list4mat = reformat2list4mat2(.newdata, tdesc)
  pred = predict(.model$learner.model, newdata = list4mat, type = 'response')
  return(as.vector(pred))
}
