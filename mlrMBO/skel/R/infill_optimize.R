# Optimizers for infill criteria

# General interface
#
# @param infill.crit [\code{function}]\cr
#   Infill criterion function.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @return [\code{data.frame}]. One proposed point that should be evaluated.

#FIXME we need other optimizers for mixed, depenent param spaces. dont forget refactorNAS then

# mean response of model
infillOptRandom = function(infill.crit, model, control, par.set, opt.path, design) {
  iterater = 0L
  repeat {
    iterater = iterater + 1L
    newdesign1 = generateDesign(control$infill.opt.random.points, par.set,
                                randomLHS, ints.as.num=TRUE, logicals.as.factor=TRUE)
    # predict on design where NAs where imputed, but return propsed points with NAs
    newdesign2 = refactorNAs(newdesign1, par.set)
    y = infill.crit(newdesign2, model, control, par.set, design)
    best = newdesign1[rank(y, ties.method="random") == 1L, , drop=FALSE]
    
    if(iterater == control$infill.opt.restarts)
      break
    
    for(i in seq(along=par.set$pars)) {
      par = par.set$pars[[i]]
      if(par$type %in% c("numeric", "integer", "numericvector", "integervector")) {
        range = par$upper - par$lower
        if(best[[i]] < par$lower + range / 4)
          par$upper = par$upper - range / 2
        else if (best[[i]] > par$upper - range / 4)
          par$lower = par$lower + range / 2
        else {
          par$lower = par$lower + range / 4
          par$upper = par$upper - range / 4
        }
        par.set$pars[[i]] = par
      }
    }
  }
  best
}

infillOptCMAES = function(infill.crit, model, control, par.set, opt.path, design) {
  # extract lower and upper bound for params
  low = getLower(par.set)
  upp = getUpper(par.set)

  rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  #eval all points of 1 generation at once
  cmaes.control = control$cmaes.control
  cmaes.control$vectorized = TRUE
  f = function(x) {
    newdata = as.data.frame(t(x))
    colnames(newdata) = rep.pids
    infill.crit(newdata, model, control, par.set, design)
  }

  results = vector("list", control$infill.opt.restarts)
  # restart optimizer, first start point is currently best
  for (i in 1:control$infill.opt.restarts) {
    if (i == 1) {
      start = getOptPathEl(opt.path, getOptPathBestIndex(opt.path))$x
    } else {
      start = sampleValue(par.set)
    }
    start = unlist(start)
    results[[i]] = cma_es(par=start, fn=f, lower=low, upper=upp, control=control$infill.opt.cmaes.control)
  }
  ys = extractSubList(results, "value")
  j = which(rank(ys, ties.method="random") == 1L)
  as.data.frame(t(results[[j]]$par))
}

# FIXME: allow DiceOptim optimizer later...
# infillOptEI = function(infill.crit, model, control, par.set, opt.path) {
#   # extract lower and upper bound for params
#   low = getLower(par.set)
#   upp = getUpper(par.set)
#
#   i = getOptPathBestIndex(opt.path, ties="random")
#   start = unlist(getOptPathEl(opt.path, i)$x)
#   capture.output(design <- max_EI(model$learner.model,
#     lower=low, upper=upp, parinit=start)$par)
#   as.data.frame(design)
# }
