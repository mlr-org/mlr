#' @title Functional linear array model bossting.
#'
#' @description
#' Learner for Functional linear array modeling boosting.
#'
#' @export
makeRLearner.fdaregr.FDboost = function() {
  makeRLearnerRegr(
    cl = "fdaregr.FDboost",
    package = c("FDboost", "mboost"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "bsignal.knots", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "bsignal.df", default = 3L, lower = 1L),
      makeUntypedLearnerParam(id = "timeformular", default = NULL),
      makeLogicalLearnerParam(id = "normalize", default = FALSE),  # whether to normalize column to fit the need of mboost
      makeLogicalLearnerParam(id = "check.indent", default = TRUE)
      ),
    properties = c("numerics"),
    name = "FLAM regression",
    short.name = "FDboost"
  )
}

#' @export
trainLearner.fdaregr.FDboost = function(.learner, .task, .subset, .weights = NULL, mstop = 100L, bsignal.knots, bsignal.df) {
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
  formula.terms = character(length(fdf))
  for (fdn in fdns) {
    gn = paste0(fdn, ".grid")
    mat.list[[fdn]]=  as.matrix(d[, tdesc$fd.features[[fdn]], drop = FALSE])
    formula.terms[fdn] = sprintf("bsignal(%s, %s, knots = %d, df = %d, check.ident = FALSE)", fdn, gn, bsignal.knots, bsignal.df)
  }
  mat.list = c(mat.list, fdg)
  mat.list[[tn]] = d[, tn]
  form = as.formula(sprintf("%s ~ %s", tn, collapse(formula.terms, "+")))
  FDboost::FDboost(formula = form, timeformula = ~bols(1), data = mat.list, control = mboost::boost_control(mstop = mstop))
}


reformat2mat.list = function(.data, tdesc){
  df =  .data
  fd.features = tdesc$fd.features
  fd.grids = tdesc$fd.grids
  tn = tdesc$target
  channel.list = tdesc$fd.features
  index.list = tdesc$fd.grids
  name4channel = names(index.list)
  num4channel = length(index.list)
  mat.list = list()
  for(i in 1:num4channel){
    mat.list[[name4channel[[i]]]]=  as.matrix(subset(df, select = channel.list[[i]]))
    mat.list[[paste0(name4channel[[i]],".index") ]]=  index.list[[i]]
  }
  return(mat.list)
}

#' @export
predictLearner.fdaregr.FDboost = function(.learner, .model, .newdata, ...) {
  mextra_para  = list(...)
  tdesc = getTaskDescription(.model)
  mat.list = reformat2mat.list(.newdata, tdesc )
  pred = predict(object = .model$learner.model, newdata = mat.list)
  return(pred)
}
