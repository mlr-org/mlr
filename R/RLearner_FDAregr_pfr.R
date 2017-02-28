#' @title Penalized functional regression.
#'
#' @description
#' Learner for penalized functional regression.
#'
#' @export
makeRLearner.fdaregr.pfr = function() {
  makeRLearnerRegr(
    cl = "fdaregr.pfr",
    package = "refund",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "af.k", default = -1L) # 50L comes from the refund documentation
    ),
    properties = c("numerics"),
    name = "penalized functional regression",
    short.name = "pfr"
  )
}

#' @export
trainLearner.fdaregr.pfr = function(.learner, .task, .subset, .weights = NULL, af.k = 1L, ...) {
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
    formula.terms[fdn] = sprintf("af(%s, basistype = 's', Qtransform = TRUE, k=%d)", fdn, af.k)
  }
  mat.list = c(mat.list, fdg)
  mat.list[[tn]] = d[, tn]
  form = as.formula(sprintf("%s ~ %s", tn, collapse(formula.terms, "+")))
  pfr(formula = form, data = mat.list)
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
  list4mat = reformat2list4mat2(.newdata, tdesc )
  pred = predict(.model$learner.model, newdata = list4mat, type = 'response')
  return(as.vector(pred))
}

