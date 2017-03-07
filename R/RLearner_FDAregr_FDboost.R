#' @title Functional linear array model boosting.
#'
#' @description
#' Learner for Functional linear array modele boosting.
#'
#' @export
makeRLearner.fdaregr.FDboost = function() {
  makeRLearnerRegr(
    cl = "fdaregr.FDboost",
    package = c("FDboost", "mboost"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "bsignal.knots", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "bsignal.df", default = 4L, lower = 1L),
      makeLogicalLearnerParam(id = "normalize", default = FALSE),  # whether to normalize column to fit the need of mboost
      makeLogicalLearnerParam(id = "check.indent", default = TRUE)  # FIXME: this is currently hard coded to be TRUE
      ),
    properties = c("numerics"),
    name = "FLAM regression",
    short.name = "FDboost"
  )
}

#' @export
trainLearner.fdaregr.FDboost = function(.learner, .task, .subset, .weights = NULL, mstop = 100L, 
  bsignal.knots = 10L, bsignal.df = 4L, ...) {
  d = getTaskData(.task, subset = .subset)
  tn = getTaskTargetNames(.task)
  tdesc = getTaskDescription(.task)
  fdf = tdesc$fd.features

  # later on, the grid elements in mat.list should have suffix ".grid"
  fdg = setNames(tdesc$fd.grids, paste0(names(tdesc$fd.grids), ".grid"))
  fdns = names(fdf)
  # setup mat.list: for each func covar we add its data matrix and its grid. and once the target col
  # also setup charvec of formula terms for func covars
  mat.list = namedList(fdns)
  formula.terms = character(length(fdf))
  for (fdn in fdns) {
    gn = paste0(fdn, ".grid")
    mat.list[[fdn]] = as.matrix(d[, tdesc$fd.features[[fdn]], drop = FALSE])
    formula.terms[fdn] = sprintf("bsignal(%s, %s, knots = %d, df = %d, check.ident = FALSE)", fdn, gn, bsignal.knots, bsignal.df)
  }
  mat.list = c(mat.list, fdg)
  mat.list[[tn]] = d[, tn]
  form = as.formula(sprintf("%s ~ %s", tn, collapse(formula.terms, "+")))
  FDboost::FDboost(formula = form, timeformula = ~bols(1), data = mat.list, control = mboost::boost_control(mstop = mstop))
}


reformat2mat.list = function(data, tdesc){
  tn = tdesc$target
  fdns = names(tdesc$fd.features)
  mat.list = namedList(fdns)
  i = 1L
  for(fdn in fdns){
    mat.list[[fdn]]=  as.matrix(subset(data, select = tdesc$fd.features[[fdn]]))
    mat.list[[paste0(fdn,".index") ]] =  tdesc$fd.grids[[i]]
    i = i + 1
  }
  return(mat.list)
}

#' @export
predictLearner.fdaregr.FDboost = function(.learner, .model, .newdata, ...) {
  mextra_para  = list(...)
  tdesc = getTaskDescription(.model)
  mat.list = reformat2mat.list(.newdata, tdesc)
  pred = predict(object = .model$learner.model, newdata = mat.list)
  return(pred)
}
