#' @export
getParamSet.BaseWrapper = function(x) {
  c(x$par.set, getParamSet(x$next.learner))
}


#' @export
getHyperPars.BaseWrapper = function(learner, for.fun = c("train", "predict", "both")) {
  c(getHyperPars(learner$next.learner, for.fun), getHyperPars.Learner(learner, for.fun))
}


#' @export
setHyperPars2.BaseWrapper = function(learner, par.vals) {
  ns = names(par.vals)
  pds.n = names(learner$par.set$pars)
  for (i in seq_along(par.vals)) {
    if (ns[i] %in% pds.n) {
      learner = setHyperPars2.Learner(learner, par.vals = par.vals[i])
    } else {
      learner$next.learner = setHyperPars2(learner$next.learner, par.vals = par.vals[i])
    }
  }
  return(learner)
}

#' @export
removeHyperPars.BaseWrapper = function(learner, ids) {
  i = intersect(names(learner$par.vals), ids)
  if (length(i) > 0L) {
    learner = removeHyperPars.Learner(learner, i)
  }
  learner$next.learner = removeHyperPars(learner$next.learner, setdiff(ids, i))
  return(learner)
}



getLeafLearner = function(learner) {
  if (inherits(learner, "BaseWrapper")) {
    return(getLeafLearner(learner$next.learner))
  }
  return(learner)
}


# default is to set the predict.type for the wrapper and recursively for all learners inside
# if one does not want this, one must override
#' @export
setPredictType.BaseWrapper = function(learner, predict.type) {
  learner$next.learner = setPredictType(learner$next.learner, predict.type)
  setPredictType.Learner(learner, predict.type)
}


#' @export
getClassWeightParam.BaseWrapper = function(learner, ...) {
  assertClass(learner, "BaseWrapper")
  weight.param.name = learner$next.learner$class.weights.param
  learner$next.learner$par.set$pars[[weight.param.name]]
}
