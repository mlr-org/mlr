tuneMultiCritNSGA2 = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {

  requirePackages("mco", why = "tuneMultiCritNSGA2", default.method = "load")
  low = getLower(par.set)
  upp = getUpper(par.set)
  cx = function(x) convertXNumeric(x, par.set)
  # FIXME: we need a vectorized version

  args = list(fn = tunerFitnFun, idim = length(low), odim = length(measures),
    lower.bounds = low, upper.bounds = upp,
    learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = cx, remove.nas = FALSE)
  args = c(args, control$extra.args)

  do.call(mco::nsga2, args)

  makeTuneMultiCritResultFromOptPath(learner, par.set, measures, control, opt.path)
}

