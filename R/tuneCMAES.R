tuneCMAES = function(learner, task, resampling, measures, par.set, control, opt.path, show.info, resample.fun) {

  requirePackages("cmaes", why = "tune_cmaes", default.method = "load")

  low = getLower(par.set)
  upp = getUpper(par.set)
  start = control$start %??% sampleValue(par.set, trafo = FALSE)
  start = convertStartToNumeric(start, par.set)
  # set sigma to 1/4 per dim, defaults in cmaes are crap for this, last time I looked
  # and vectorized evals for speed and parallel, then insert user controls
  # FIXME: there is a bug in cmaes that I reported MULTIPLE times now
  # while the docs say we can set sigma to a vector, there is a stopifnot in code which does not allow it
  sigma = median(upp - low) / 2
  ctrl.cmaes = list(vectorized = TRUE, sigma = sigma)
  ctrl.cmaes = insert(ctrl.cmaes, control$extra.args)

  # check whether the budget parameter is used correctly;
  # this check is only performed, if the budget is defined, but neither start, lambda nor maxit were defined
  N = length(start)
  budget = control$budget

  # either use user choice or lambda default, now lambda is set
  if (is.null(ctrl.cmaes$lambda)) {
    ctrl.cmaes$lambda = 4 + floor(3 * log(N))
  }

  # if we have budget, calc maxit, otherwise use CMAES default, now maxit is set
  maxit = if (is.null(budget)) {
    ifelse(is.null(ctrl.cmaes$maxit), 100 * N^2, ctrl.cmaes$maxit)
  } else {
    floor(budget / ctrl.cmaes$lambda)
  }

  if (!is.null(budget) && budget < ctrl.cmaes$lambda) {
    stopf("Budget = %$i cannot be less than lambda = %i!", budget, ctrl.cmaes$lambda)
  }

  if (!is.null(ctrl.cmaes$maxit) && ctrl.cmaes$maxit != maxit) {
    stopf("Provided setting of maxit = %i does not work with provided budget = %s, lambda = %i",
      ctrl.cmaes$maxit, ifelse(is.null(budget), "NULL", budget), ctrl.cmaes$lambda)
  }
  ctrl.cmaes$maxit = maxit

  cmaes::cma_es(par = start, fn = tunerFitnFunVectorized, lower = low, upper = upp, control = ctrl.cmaes,
    learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = convertXVectorizedMatrixCols, remove.nas = FALSE, resample.fun = resample.fun)

  makeTuneResultFromOptPath(learner, par.set, measures, resampling, control, opt.path)
}
