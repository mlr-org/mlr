checkTunerParset = function(learner, par.set, control) {
  if (length(par.set$pars) == 0)
    stop("No parameters were passed!")
  x = setdiff(names(par.set$pars), names(getParamSet(learner)$pars))
  if (length(x) > 0)
    stop("Can only tune parameters for which learner parameters exist: ", paste(x, collapse=","))
  
  
  checkParsOk = function(algo, ok) {
    if(length(filterParams(par.set, type=ok)$pars) < length(par.set$pars))
      stop(sprintf("%s can only be applied to: %s!", algo, paste(ok, collapse=",")))
  }
  checkStart = function() {
    if (length(control$start) != length(par.set$pars))
      stop("Length of 'start' has to match number of parameters in 'par.set'!")
    x = setdiff(names(control$start), names(getParamSet(learner)$pars))
    if (length(x) > 0)
      stop("'start' contains parameters for which no learner parameters exist: ", paste(x, collapse=","))
  }
  
  if (is(control, "TuneControlGrid")) {
    checkParsOk("Grid search",  c("discrete", "logical"))
  } else if (is(control, "TuneControlOptim")) {
    checkParsOk("Optim", c("numeric", "integer", "numericvector", "integervector"))
    checkStart()
  } else if (is(control, "TuneControlCMAES")) {
    checkParsOk("CMAES", c("numeric", "integer", "numericvector", "integervector"))
    checkStart()
  } else if (is(control, "TuneControlMBO")) {
  } else if (is(control, "TuneControlMies")) {
  } else if (is(control, "TuneControlIrace")) {
  } else if (is(control, "TuneControlRandom")) {
  } else {
    stop("Tuning algorithm for ", class(control)[1], " does not exist!")
  }
  if(any(sapply(par.set$pars, function(p) !is.null(p$requires)))) {
    if (!(is(control, "TuneControlRandom") || is(control, "TuneControlIrace"))) 
      stop("Tuning algorithm for ", class(control)[1], " cannot handle dependent paramters!")
  }
  
}
