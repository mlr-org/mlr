# FIXME:  set initial varaince to (upper-lower)/2 if both bounds are given
tuneCMAES = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {

  requirePackages("cmaes", "tune_cmaes")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start
  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  ctrl.cmaes = control$extra.args
  ctrl.cmaes$vectorized = TRUE
  cx = function(x) convertXMatrixCols(x, par.set)

  or = cma_es(par = start, fn = tunerFitnFunVectorized, lower = low, upper = upp, control = ctrl.cmaes,
    learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    trafo = TRUE, convertx = cx, remove.nas = FALSE)

  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties = "random")
  e = getOptPathEl(opt.path, i)
  makeTuneResult(learner, control, e$x, e$y, opt.path)
}
