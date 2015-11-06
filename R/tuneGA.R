tuneGA = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("GA", why = "tuneGA", default.method = "load")
  
  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start
  
  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)

  cx = function(x) convertXNumeric(x, par.set)
  ctrl.ga = control$extra.args
 
  if (is.null(ctrl.ga$pcrossover)) {
    ctrl.ga$pcrossover = 0.8
  }

  if (is.null(ctrl.ga$pmutation)) {
    ctrl.ga$pmutation = 0.1
  }

  if (is.null(ctrl.ga$parallel)) {
    ctrl.ga$parallel = FALSE
  }
    
  if (is.null(ctrl.ga$popSize)) {
    ctrl.ga$popSize = 50L
  }

  if (is.null(ctrl.ga$maxit)) {
    ctrl.ga$maxit = 100L
  }

  ctrl.ga$run = ctrl.ga$maxit
  maxf = ctrl.ga$popSize * ctrl.ga$maxit

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
    max = upp, maxiter = ctrl.ga$maxit, run = ctrl.ga$run, popSize = ctrl.ga$popSize,
    pcrossover = ctrl.ga$pcrossover, pmutation = ctrl.ga$pmutation, suggestions = start, 
    parallel = ctrl.ga$parallel, monitor = NULL)

  tune.result = makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
  return(tune.result)
}