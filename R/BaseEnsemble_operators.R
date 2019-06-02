# find the learner for a given param name, so <learnerid>.<paramid>
matchBaseEnsembleLearner = function(ensemble, pn) {
  patterns = stri_paste("^", names(ensemble$base.learners), "\\.")
  j = which(vlapply(patterns, stri_detect_regex, str = pn))
  par.id = stri_replace_first(pn, "", regex = patterns[j])
  list(index = j, par.id = par.id)
}

#' @export
getHyperPars.BaseEnsemble = function(learner, for.fun = c("train", "predict", "both")) {
  pvs = lapply(learner$base.learners, function(lrn) {
    xs = getHyperPars(lrn, for.fun = for.fun)
    if (length(xs) > 0L) {
      names(xs) = stri_paste(lrn$id, ".", names(xs))
    }
    return(xs)
  })
  # if we dont do this, R prefixes the list names again.
  # I rather want to control this explicitly, who know about the special cases...
  names(pvs) = NULL
  pvs = do.call(c, pvs)
  c(pvs, learner$par.vals)
}

# set hyper pars down in ensemble base learners, identify correct base learner + remove prefix
#' @export
setHyperPars2.BaseEnsemble = function(learner, par.vals) {
  ns = names(par.vals)
  parnames.bls = names(learner$par.set.bls$pars)
  for (i in seq_along(par.vals)) {
    pn = ns[i]
    if (pn %in% parnames.bls) {
      # param of ensapsulated learner, remove prefix, set it in the bl list
      z = matchBaseEnsembleLearner(learner, pn)
      learner$base.learners[[z$ind]] = setHyperPars2(learner$base.learners[[z$ind]],
        par.vals = setNames(par.vals[i], z$par.id))
    } else {
      # extra param of ensemble learner, just set it normally
      learner = setHyperPars2.Learner(learner, par.vals = par.vals[i])
    }
  }
  return(learner)
}

#' @export
removeHyperPars.BaseEnsemble = function(learner, ids) {
  parnames.bls = names(learner$par.set.bls$pars)
  for (id in ids) {
    if (id %in% parnames.bls) {
      # param of ensapsulated learner, remove prefix, set it in the bl list
      z = matchBaseEnsembleLearner(learner, id)
      # FIXME: won't work properly when base.learners are BaseWrappers, should we support this?
      learner$base.learners[[z$ind]] = removeHyperPars(learner$base.learners[[z$ind]],
        z$par.id)
    } else {
      # extra param of ensemble learner, just remove it normally
      learner = removeHyperPars.Learner(learner, id)
    }
  }
  return(learner)
}

# default is to set the predict.type for the BaseEnsemble and for all base learners inside
# if one does not want this, one must override
#' @export
setPredictType.BaseEnsemble = function(learner, predict.type) {
  # this does the check for the prop
  lrn = setPredictType.Learner(learner, predict.type)
  lrn$base.learners = lapply(lrn$base.learners, setPredictType, predict.type = predict.type)
  return(lrn)
}

#' @export
makeWrappedModel.BaseEnsemble = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod(x)
  addClasses(x, "BaseEnsembleModel")
}

#' @export
getClassWeightParam.BaseEnsemble = function(learner, lrn.id = NULL) {
  assertClass(learner, "BaseEnsemble")

  bl.ids = vcapply(learner$base.learners, getLearnerId)
  if (is.null(lrn.id)) {
    stopf("'lrn.id' is not set, please specify one of the base learners: %s", stri_flatten(bl.ids, ", "))
  }
  if (lrn.id %nin% bl.ids) {
    stopf("%s is not a base learner. Available base learners are: %s", lrn.id, stri_flatten(bl.ids, ", "))
  }

  getClassWeightParam(learner$base.learners[[lrn.id]])
}
