#' @export
getParamSet.BaseEnsemble = function(learner) {
  c(learner$par.set, getParamSet(learner$next.learner))
}

#' @export
getHyperPars.BaseEnsemble = function(learner, for.fun = "train") {
  lrns = c(learner, learner$base.learners)
  pvs = lapply(lrns, geHyperPars.Learner, for.fun = for.fun)
  do.call(c, pvs)
}

#' @export
setHyperPars2.BaseEnsemble = function(learner, par.vals) {
  ns = names(par.vals)
  pds.n = names(learner$par.set$pars)
  for (i in seq_along(par.vals)) {
    if (ns[i] %in% pds.n) {
      learner = setHyperPars2.Learner(learner, par.vals = par.vals[i])
    } else {
      learner$base.learners = lapply(learner$base.learners, setHyperPars2, par.vals = par.vals[i])
    }
  }
  return(learner)
}

#' @export
removeHyperPars.BaseEnsemble = function(learner, ids) {
  i = intersect(names(learner$par.vals), ids)
  if (length(i) > 0L)
    learner = removeHyperPars.Learner(learner, i)
  learner$next.learner = removeHyperPars.Learner(learner$next.learner, setdiff(ids, i))
  return(learner)
}

# default is to set the predict.type for the BaseEnsemble and for all base learners inside
# if one does not want this, one must override
#' @export
setPredictType.BaseEnsemble = function(learner, predict.type) {
  learner$base.learners = lapply(learner$base.learners, setPredictType,  predict.type = predict.type)
  setPredictType.Learner(learner, predict.type)
}

