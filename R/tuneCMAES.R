tuneCMAES = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {

  requirePackages("cmaes", "tune_cmaes")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start
  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  # set sigma to 1/4 per dim, defaults in cmaes are crap for this, last time I looked
  # and vectorized evals for speed and parallel, then insert user controls
  # FIXME: there is a bug in cmaes that I reported MULTIPLE times now
  # while the docs say we can set sigma to a vector, there is a stopifnot in code which does not allow it
  sigma = median(upp - low) / 2
  ctrl.cmaes = list(vectorized = TRUE, sigma = sigma)
  ctrl.cmaes = insert(ctrl.cmaes, control$extra.args)
  cx = function(x) convertXMatrixCols(x, par.set)

  or = cmaes::cma_es(par = start, fn = tunerFitnFunVectorized, lower = low, upper = upp, control = ctrl.cmaes,
    learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = cx, remove.nas = FALSE)

  makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
}
