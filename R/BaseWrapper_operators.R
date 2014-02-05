#' @S3method getParamSet BaseWrapper
getParamSet.BaseWrapper = function(learner) {
  c(learner$par.set, getParamSet(learner$next.learner))
}


#' @S3method getHyperPars BaseWrapper
getHyperPars.BaseWrapper = function(learner, for.fun="train") {
  x = getHyperPars.Learner(learner, for.fun)
  c(getHyperPars(learner$next.learner, for.fun), getHyperPars.Learner(learner, for.fun))
}


#' @S3method setHyperPars2 BaseWrapper
setHyperPars2.BaseWrapper = function(learner, par.vals) {
  ns = names(par.vals)
  pds.n = names(learner$par.set$pars)
  for (i in seq_along(par.vals)) {
    if (ns[i] %in% pds.n) {
      learner = setHyperPars2.Learner(learner, par.vals=par.vals[i])
    } else {
      learner$next.learner = setHyperPars2(learner$next.learner, par.vals=par.vals[i])
    }
  }
  return(learner)
}


getLeafLearner = function(learner) {
  if (inherits(learner, "BaseWrapper"))
    return(getLeafLearner(learner$next.learner))
  return(learner)
}
