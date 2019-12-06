# check this:
# - tune algo exists
# - parameters are not empty
# - threshold tuning can be done
# - algo can handle these parameters
# - algo can handle dependencies

checkTunerParset = function(learner, par.set, measures, control) {

  cl = getClass1(control)

  if (getParamNr(par.set) == 0L) {
    stop("No parameters were passed!")
  }

  x = setdiff(names(par.set$pars), names(getParamSet(learner)$pars))
  if (length(x) > 0L) {
    stopf("Can only tune parameters for which learner parameters exist: %s", collapse(x))
  }

  checkParsOk = function(algo, ok) {
    if (length(filterParams(par.set, type = ok)$pars) < length(par.set$pars)) {
      stopf("%s can only be applied to: %s!", algo, collapse(ok))
    }
  }

  checkStart = function() {
    if (!is.null(control$start)) {
      if (length(control$start) != length(par.set$pars)) {
        stop("Length of 'start' has to match number of parameters in 'par.set'!")
      }
      x = setdiff(names(control$start), names(getParamSet(learner)$pars))
      if (length(x)) {
        stopf("'start' contains parameters for which no learner parameters exist: %s", collapse(x))
      }
    }
  }

  if (control$tune.threshold && (learner$type != "classif" || learner$predict.type != "prob")) {
    stop("Using 'tune.threshold' requires a classif learner with predict.type = 'prob'!")
  }

  # check special conditions for some tuners
  if (inherits(control, "TuneControlCMAES")) {
    checkParsOk("CMAES", c("numeric", "integer", "numericvector", "integervector"))
    checkStart()
  }
  if (inherits(control, "TuneControlGenSA")) {
    checkParsOk("GenSA", c("numeric", "integer", "numericvector", "integervector"))
    checkStart()
  }
  if (inherits(control, "TuneControlNSGA2")) {
    checkParsOk("NSGA2", c("numeric", "integer", "numericvector", "integervector"))
  }

  # check requires / dependent params
  if (hasRequires(par.set) && cl %nin% c("TuneControlRandom", "TuneControlGrid",
    "TuneControlDesign", "TuneControlIrace", "TuneControlMBO", "TuneMultiCritControlRandom",
    "TuneMultiCritControlMBO")) {
    stopf("Tuning algorithm for '%s' cannot handle dependent parameters!", cl)
  }

  if (inherits(control, "TuneMultiCritControl")) {
    if (length(control$impute.val) != length(measures)) {
      stop("Length of 'impute.val' must coincide with number of measures!")
    }
  }


}
