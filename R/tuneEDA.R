tuneEDA = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("copulaedas", why = "tuneEDA", default.method = "load")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start

  if (is.null(start))
    start = sampleValue(par.set, start, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  
  ctrl.eda = control$extra.args
  cx = function(x) convertXNumeric(x, par.set)
 
  if (is.null(ctrl.eda$popSize)) {
    ctrl.eda$popSize = 100L
  }

  if (is.null(ctrl.eda$maxit)) {
    ctrl.eda$maxit = 50L
  }

  if (is.null(ctrl.eda$type) || ctrl.eda$type %nin% c("UMDA", "GCEDA", "CVEDA", "DVEDA")) {
    ctrl.eda$type = "UMDA"
  }

  maxf = ctrl.eda$popSize * ctrl.eda$maxit

  if (is.null(control$budget)) {
      control$budget = maxf
  } else {
    if (maxf != control$budget) {
      stopf("The given budget (%i) contradicts to the maximum number of function evaluations (maxf = %i).", 
        control$budget, maxf)
    }
  }

  if (ctrl.eda$type == "UMDA") {
	  model = copulaedas::CEDA(copula = "indep", margin = "truncnorm", popSize = ctrl.eda$popSize, 
	    maxGens = ctrl.eda$maxit)
  } else if (ctrl.eda$type == "GCEDA") {
	  model = copulaedas::CEDA(copula = "normal", margin = "truncnorm", popSize = ctrl.eda$popSize, 
	    maxGens = ctrl.eda$maxit)
  } else if (ctrl.eda$type == "CVEDA") {
  	model = copulaedas::VEDA(vine = "CVine", indepTestSigLevel = 0.01, copulas = c("normal"), 
  	  margin = "truncnorm", popSize = ctrl.eda$popSize, maxGens = ctrl.eda$maxit)
  } else {
 	  model = copulaedas::VEDA(vine = "DVine", indepTestSigLevel = 0.01, copulas = c("normal"), 
 	    margin = "truncnorm", popSize = ctrl.eda$popSize, maxGens = ctrl.eda$maxit)
  }

  copulaedas::edaSeedUniform(model, lower = low, upper = upp)

  tunerFitFunWrapper = function(learner = learner, tasks = task, resampling = resampling, 
    measures = measures, par.set = par.set, ctrl = ctrl, opt.path = opt.path, show.info = show.info,
    convertx = convertx, remove.nas = remove.nas) {
  	temp = function(x) {
		return( tunerFitnFun (x, learner, task, resampling, measures, par.set, ctrl, opt.path, 
  	  show.info, convertx, remove.nas))
  	}
  }
  	
  res = copulaedas::edaRun(model, f = tunerFitFunWrapper(learner = learner, task = task, 
  	resampling = resampling, measures = measures, par.set = par.set, ctrl = control, 
    opt.path = opt.path, show.info = show.info, convertx = cx, remove.nas = FALSE), 
    lower = low, upper = upp)

  tune.result = makeTuneResultFromOptPath(learner, par.set, measures, control, opt.path)
  return(tune.result)
}