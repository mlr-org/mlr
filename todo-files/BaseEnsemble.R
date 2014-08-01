# an enseble of base learners (probably of different learner types) that are some how
# combined and used in combination in the derived class
#
# - properties is intersection of props of base.learners
# - required packages is union of base.learners packages
# - par.set is join of base.learner par.sets + extra params of derived class
# - predict.type = response by default
makeBaseEnsemble = function(id, base.learners, bls.type = NULL, ens.type = NULL, package = character(1L),
  par.set = makeParamSet(), par.vals = list(), cl) {

  assertString(id)
  assertVector(base.learners, min.len = 1L)
  base.learners = lapply(base.learners, checkLearner, type = bls.type)
  ids = unique(extractSubList(base.learners, "id"))
  if (length(ids) != length(base.learners))
    stop("Base learners must all have unique ids!")

  #FIXME:
  ns = intersect(names(par.set$pars), names(next.learner$par.set$pars))
  if (length(ns) > 0L)
    stopf("Hyperparameter names in wrapper clash with base learner names: %s", collapse(ns))

  # join all parsets of base.learners + prefix param names with base learner id
  par.set.bls = makeParamSet()
  for (i in seq_along(base.learners)) {
    ps = base.learners[[i]]$par.set
    pids = sprintf("%s.%s", ids[i], names(ps$pars))
    for (j in seq_along(ps$pars))
      ps$pars[[j]]$id = pids[[j]]
    names(ps$pars) = pids
    par.set.bls = c(par.set.bls, ps)
  }

  lrn = makeS3Obj(c("BaseEnsemble", "Learner"),
    id = id,
    type = type,
    package = unique(extractSubList(base.learners, "package")),
    par.set = par.set,
    par.vals = par.vals,
    properties = Reduce(intersect, extractSubList(base.learners, "properties", simplify = FALSE)),
    predict.type = "response"
  )

  lrn$base.learners = setNames(base.learners, ids)
  return(lrn)
}

predictBaseLearners = function(learner) {
  bms = .model$learner.model$base.models
  k = length(bms)
  probs = as.data.frame(matrix(NA, nrow = nrow(.newdata), ncol = k))
  colnames(probs) = extractSubList(.learner$base.learners, "id")

  # predict prob vectors with each base model
  for (i in 1:k) {
    pred = predict(bms[[i]], newdata = .newdata)
    probs[,i] = getProbabilities(pred)
  }
}

# FIXME: test
#' @export
print.BaseWrapper = function(x, ...) {
  s = ""
  y = x
  while (inherits(y, "BaseWrapper")) {
    s = paste(s, class(y)[1L], "->", sep = "")
    y = y$next.learner
  }
  s = paste(s, class(y)[1L])
  print.Learner(x)
}

