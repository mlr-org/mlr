# an ensemble of base learners (probably of different learner types) that are some how
# combined and used in combination in the derived class
#
# - we check that baselearners have a certain, unique learner type, unique ids (so we can indes with ids)
#   and set the learner type of the ensemble by default to the type of the baselearners
# - properties is intersection of props of base.learners
# - required packages is union of base.learners packages
# - par.set is join of base.learner par.sets + extra params of derived class
# - all params of base.learners get new name <learnerid>.<paramid> to avoid name clashes
# - predict.type = response by default
# - train and predict we cannot really define, must be done in derived class
makeBaseEnsemble = function(id, base.learners, bls.type = NULL,
  ens.type = NULL, package = character(0L),
  par.set = makeParamSet(), par.vals = list(), cl) {

  assertString(id)
  assertVector(base.learners, min.len = 1L)
  base.learners = lapply(base.learners, checkLearner, type = bls.type)

  tt = unique(extractSubList(base.learners, "type"))
  if (length(tt) > 1L) {
    stopf("Base learners must all be of same type, but have: %s", collapse(tt))
  }
  if (is.null(ens.type)) {
    ens.type = tt
  }

  ids = unique(extractSubList(base.learners, "id"))
  if (length(ids) != length(base.learners)) {
    stop("Base learners must all have unique ids!")
  }

  # check that all predict.types are the same
  pts = unique(extractSubList(base.learners, "predict.type"))
  if (length(pts) > 1L) {
    stopf("Base learners must all have same predict.type, but have: %s", collapse(pts))
  }

  # join all parsets of base.learners + prefix param names with base learner id
  # (we could also do this operation on-the.fly in getParamSet.BaseEnsemble,
  # like we do in getParamSet.BaseWrapper, but this would require expensive (?)
  # recomputation of the joined parset each time we need it, I guess.
  # as long as we do not change the structure of the baselearners
  # after construction of the ensemble we should be fine)
  par.set.bls = makeParamSet()
  for (i in seq_along(base.learners)) {
    ps = getParamSet(base.learners[[i]])
    pids = sprintf("%s.%s", ids[i], names(ps$pars))
    for (j in seq_along(ps$pars)) {
      ps$pars[[j]]$id = pids[[j]]
    }
    names(ps$pars) = pids
    par.set.bls = c(par.set.bls, ps)
  }
  par.set.all = c(par.set.bls, par.set)

  lrn = makeLearnerBaseConstructor(c(cl, "BaseEnsemble"),
    id = id,
    type = ens.type,
    package = unique(unlist(extractSubList(base.learners, "package"))),
    par.set = par.set.all,
    par.vals = par.vals,
    properties = Reduce(intersect, lapply(base.learners, getLearnerProperties)),
    predict.type = "response")

  lrn$base.learners = setNames(base.learners, ids)
  lrn$par.set.bls = par.set.bls
  lrn$par.set.ens = par.set
  return(lrn)
}
