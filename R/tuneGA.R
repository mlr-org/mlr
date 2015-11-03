# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

tuneGA = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("GA", why = "tuneGA", default.method = "load")
  
  #getting configuration
  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start
  
  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  
  ctrl.ga = control$extra.args
  cx = function(x) convertXNumeric(x, par.set)

  # Ga execution
  res = GA::ga(type = "real-valued", fitness = tunerFitnFun, learner = learner, task = task, 
    resampling = resampling, measures = measures, par.set = par.set, ctrl = control, 
    opt.path = opt.path, show.info = show.info, convertx = cx, remove.nas = FALSE, min = low, 
    max = upp, maxiter = ctrl.ga$maxit, run = ctrl.ga$run, popSize = ctrl.ga$popSize,
    suggestions = start, parallel = ctrl.ga$parallel, monitor = NULL)

  # Returning values
  tune.result = makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
  return(tune.result)
}

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------