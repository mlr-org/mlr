#' @title Penalized functional regression
#'
#' @description Learner for penalized functional regression
#'
#' @export
makeRLearner.fdaregr.pfr = function() {
  makeRLearnerRegr(
    cl = "fdaregr.pfr",
    package = "refund",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 50L), # 50L comes from the refund documentation
      makeUntypedLearnerParam(id = "index.list", default = NULL)
    ),
    properties = c("numerics"),
    name = "penalized functional regression",
    short.name = "pfr"
  )
}

#' @export
trainLearner.fdaregr.pfr = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  df = d$data
  tn = getTaskTargetNames(.task)
  tdesc = getTaskDescription(.task)
  index.list = tdesc$fd.grids
  channel.list = tdesc$fd.features
  name4channel = names(index.list)
  num4channel = length(index.list)
  mextra_para = list(...)
  ###FIXME: there should be a function to complete the list4mat
  ####list(...)[names(list(...))%in%names(formals)]
  list4mat = list()
  list4formula = list()
  list4mat[[eval(tn)]] = d$target
  for(i in 1:num4channel){
    list4mat[[name4channel[[i]]]]=  as.matrix(subset(df, select = channel.list[[i]]))
    list4mat[[paste0(name4channel[[i]],".index") ]]=  index.list[[i]]
    list4formula[[i]] = sprintf("af(%s, basistype = 's', Qtransform = TRUE, k=%d)", name4channel[[i]], mextra_para$k)
  }
  makeformula = function(x1,x2){
    paste(x1,x2, sep = "+")
  }
  rformula = Reduce(f = makeformula, x = list4formula)
  mformula = as.formula(paste0(tn,"~", rformula))
  ###
  fit.af.s = pfr(formula = mformula, data=list4mat)
  #fit.af.s <- pfr(tn ~ af(cca, basistype="s", Qtransform=TRUE, k=50), data=list4mat)
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
  predict(.model$learner.model, newdata = list4mat, type = 'response')
}