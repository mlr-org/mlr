tunePSO = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("pso", why = "tunePSO", default.method = "load")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start

  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)

  cx = function(x, par.set) convertXNumeric(x, par.set)
  ctrl.pso = list(trace = 0, trace.stats = NULL)
  ctrl.pso = insert(ctrl.pso, control$extra.args)
  assertInt(x = ctrl.pso$n.particles, lower = 10, null.ok = TRUE, .var.name = "n.particles")

  if(is.null(ctrl.pso$maxit))
    ctrl.pso$maxit = 100L

  if (is.null(ctrl.pso$n.particles)) {
    if (ctrl.pso$pso.impl == "SPSO2011") {
      ctrl.pso$s = 40L
    } else {
      ctrl.pso$s = floor(10+2*sqrt(length(par.set)))
    }
  } else {
    ctrl.pso$s = ctrl.pso$n.particles
    # cannot be in the control or package 'pso' throws a warning
    ctrl.pso$n.particles = NULL
  }
  # cannot be in the control or package 'pso' throws a warning
  ctrl.pso$pso.impl = NULL
  
  ctrl.pso$maxf = (ctrl.pso$s * ctrl.pso$maxit)
  if (is.null(control$budget)){
    control$budget = ctrl.pso$maxf
  } else {
    if(ctrl.pso$maxf != control$budget) {
      stopf("The given budget (%i) contradicts to the maximum number of function evaluations (maxf = %i).",
      control$budget, ctrl.pso$maxf)
    }
  }

  res = pso::psoptim(par = start, fn = tunerFitnFun, learner = learner, task = task,
    resampling = resampling, measures = measures, par.set = par.set, ctrl = control,
    opt.path = opt.path, show.info = show.info, convertx = cx, remove.nas = FALSE,
    lower = low, upper = upp, control = ctrl.pso)

  tune.result = makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
  return(tune.result)
}
