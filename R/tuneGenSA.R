tuneGenSA = function(learner, task, resampling, measures, par.set, control, opt.path, show.info, resample.fun) {

  requirePackages("GenSA", why = "tuneGenSA", default.method = "load")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start %??% sampleValue(par.set, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  ctrl.gensa = control$extra.args

  # FIXME: GenSA exceeds the given budget (if it is very low)!
  res = GenSA::GenSA(par = start, fn = tunerFitnFun, lower = low, upper = upp, control = ctrl.gensa,
    learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = convertXNumeric, remove.nas = FALSE, resample.fun = resample.fun)

  # FIXME: the following condition can be removed, once we are able to fix the
  # budget in GenSA
  if (!is.null(control$budget) && res$counts > control$budget) {
    warningf("GenSA used %i function calls, exceededing the given budget of %i evaluations.",
      res$counts, control$budget)
  }

  makeTuneResultFromOptPath(learner, par.set, measures, resampling, control, opt.path)
}
