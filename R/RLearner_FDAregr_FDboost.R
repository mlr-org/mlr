#' @title Functional linear array model bossting
#'
#' @description Learner for Functional linear array modeling boosting
#'
#' @export
makeRLearner.fdaregr.FDboost = function() {
  makeRLearnerRegr(
    cl = "fdaregr.FDboost",
    package = "FDboost",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "mstop", default = 200L),
      makeIntegerLearnerParam(id = "degree4freedom", default = 3L, lower = 1L),
      makeIntegerLearnerParam(id = "num_knots", default = 40L, lower = 1L),
      makeUntypedLearnerParam(id = "timeformular", default = NULL),
      makeUntypedLearnerParam(id = "index.list", default = NULL),
      makeLogicalLearnerParam(id = "normalize", default = FALSE),  # whether to normalize column to fit the need of mboost
      makeLogicalLearnerParam(id = "check.indent", default = TRUE)  
      ),
    properties = c("numerics"),
    name = "FLAM regression",
    short.name = "FDboost"
  )
}

#' @export
trainLearner.fdaregr.FDboost = function(.learner, .task, .subset, .weights = NULL, ...) {
  fdboost.task = .task
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  tn = getTaskTargetNames(.task)
  df = d$data
  tdesc = getTaskDescription(.task)
  channel.list = tdesc$fd.features
  index.list = tdesc$fd.grids
  z = d$target
  mextra_para  = list(...)
  name4channel = names(index.list)
  num4channel = length(index.list)
  list4mat = list()
  list4formula = list()
  list4mat[[eval(tn)]] = d$target
  for(i in 1:num4channel){
    list4mat[[name4channel[[i]]]]=  as.matrix(subset(df, select = channel.list[[i]]))
    list4mat[[paste0(name4channel[[i]],".index") ]]=  index.list[[i]]
    list4formula[[i]] = sprintf("bsignal(%s,%s,knots = %d, df = %d, check.ident = FALSE)", name4channel[[i]], paste0(name4channel[[i]],".index"), mextra_para$num_knots, mextra_para$degree4freedom)
  }
  makeformula = function(x1,x2){
    paste(x1,x2, sep = "+")
  }
  rformula = Reduce(f = makeformula, x = list4formula)
  mformula = as.formula(paste0(tn,"~", rformula))
  #form = as.formula(sprintf("%s ~ %s", tn, collapse(c(ff1, ff2), "+")))
  mod2f <- FDboost::FDboost(formula = mformula, timeformula = ~bols(1), data = list4mat, control = boost_control(mstop = 200))
  return(mod2f)
}


reformat2list4mat = function(.data, tdesc){
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
    list4mat[[paste0(name4channel[[i]],".index") ]]=  index.list[[i]]
  }
  return(list4mat)
}
  
#' @export
predictLearner.fdaregr.FDboost = function(.learner, .model, .newdata, ...) {
  mextra_para  = list(...)
  tdesc = getTaskDescription(.model)
  list4mat = reformat2list4mat(.newdata, tdesc )
  pred = predict(object = .model$learner.model, newdata = list4mat)
  return(pred)
}