tuneGA = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("GA", why = "tuneGA", default.method = "load")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start

  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)

  cx = function(x, par.set) convertXNumeric(x, par.set)
  ctrl.ga = control$extra.args
  maxf = ctrl.ga$pop.size  * ctrl.ga$maxit

  if (is.null(control$budget)) {
    control$budget = maxf
  } else {
    if (maxf != control$budget) {
      stopf("The given budget (%i) contradicts to the maximum number of function evaluations (maxf = %i).",
        control$budget, maxf)
    }
  }

  res = GA::ga(type = "real-valued", fitness = tunerFitnFun, learner = learner, task = task,
    resampling = resampling, measures = measures, par.set = par.set, ctrl = control,
    opt.path = opt.path, show.info = show.info, convertx = cx, remove.nas = FALSE, min = low,
    max = upp, maxiter = ctrl.ga$maxit, run = ctrl.ga$maxit, popSize = ctrl.ga$pop.size ,
    pcrossover = ctrl.ga$prob.crossover, pmutation = ctrl.ga$prob.mutation, suggestions = start,
    monitor = NULL)

  tune.result = makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
  return(tune.result)
}
