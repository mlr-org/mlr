tuneGenSA = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("GenSA", why = "tuneGenSA")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start
  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  ctrl.gensa = control$extra.args
  cx = function(x) convertXNumeric(x, par.set)
  GenSA::GenSA(par = start, fn = tunerFitnFun, lower = low, upper = upp, control = ctrl.gensa,
    learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = cx, remove.nas = FALSE)
  makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
}
