# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

tunePSO = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("pso", why = "tunePSO", default.method = "load")
  
  # Getting configuration
  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start
  
  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  
  ctrl.pso = control$extra.args
  cx = function(x) convertXNumeric(x, par.set)

  # PSO execution
  res = pso::psoptim(par = start, fn = tunerFitnFun, learner = learner, task = task, 
    resampling = resampling, measures = measures, par.set = par.set, ctrl = control, 
    opt.path = opt.path, show.info = show.info, convertx = cx, remove.nas = FALSE, 
    lower = low, upper = upp, control = list(maxit = ctrl.pso$maxit, s = ctrl.pso$nParticles,
    type = ctrl.pso$type, maxf = ctrl.pso$maxf, trace = ctrl.pso$trace, 
    trace.stats = ctrl.pso$trace.stats))
  
  if( !is.null(control$budget) && (res$counts[1] > control$budget)) {
    warningf("PSO used %i function calls, exceededing the given budget of %i evaluations.",
      res$counts[1], control$budget)
  }

  # Returning values
  tune.result = makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
  return(tune.result)
}

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------