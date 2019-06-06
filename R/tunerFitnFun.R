# generates an R function that we can pass to a tuner to optrimize hyper pars
# - possibly convert x with custom functon
# - eval states (maybe in parallel)
# - add evals to opt path
# - return y scalar (vector for vectorized version below), always minimized
# @param always.minimize [logical(1)]
#   If TRUE, y will be tranformed based on the measure to always make sure that the desired optimum is the minimum.
# one x
tunerFitnFun = function(x, learner, task, resampling, measures, par.set, ctrl,
  opt.path, show.info, convertx, remove.nas, resample.fun, always.minimize = TRUE) {

  x = convertx(x, par.set)
  # transform parameters
  dob = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  res = evalOptimizationState(learner, task, resampling, measures, par.set, NULL, ctrl,
    opt.path, show.info, dob, x, remove.nas, resample.fun)
  extra = getTuneThresholdExtra(ctrl, res)
  # include error dumps only when at least one dump is present. (this only happens
  # when options tell us to save dumps).
  if (getMlrOption("on.error.dump")) {
    if (is.null(extra)) {
      extra = list()
    }
    extra$.dump = res$err.dumps
  }
  addOptPathEl(opt.path, x = x, y = res$y, dob = dob, eol = NA, check.feasible = TRUE,
    exec.time = res$exec.time, error.message = res$errmsg, extra = extra)
  convertYForTuner(res$y, measures, ctrl, always.minimize)
}

tunerSmoofFun = function(learner, task, resampling, measures, par.set, ctrl, opt.path, show.info, convertx, remove.nas, resample.fun) {

  measures = ensureVector(measures, n = 1L, cl = "Measure")

  # remove trafos for mbo, we do this in tunerFitnFun
  ps2 = par.set
  for (i in seq_along(ps2$pars)) {
    ps2$pars[[i]]$trafo = NULL
  }


  fn = function(x) {
    # tell smoof the optimization direction, don't transform y later
    tunerFitnFun(x, learner, task, resampling, measures, par.set, ctrl, opt.path, show.info, convertx, remove.nas, resample.fun, always.minimize = FALSE)
  }
  if ("TuneMultiCritControlMBO" %in% class(ctrl)) {
    requirePackages("smoof", why = "TuneMultiCritControlMBO", default.method = "load")
    # FIXME: Optimization of noisy multi-objective functions not supported at the moment by mlrMBO
    fun = smoof::makeMultiObjectiveFunction(fn = fn, par.set = ps2, has.simple.signature = FALSE,
      n.objectives = ctrl$mbo.control$n.objectives, minimize = extractSubList(measures, "minimize"))
  } else {
    fun = smoof::makeSingleObjectiveFunction(fn = fn, par.set = ps2, has.simple.signature = FALSE, noisy = TRUE, minimize = measures[[1]]$minimize)
  }
  return(fun)
}

# multiple xs in parallel
tunerFitnFunVectorized = function(xs, learner, task, resampling, measures, par.set, ctrl,
  opt.path, show.info, convertx, remove.nas, resample.fun) {
  xs = convertx(xs, par.set)
  dob = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  res.list = evalOptimizationStatesTune(learner, task, resampling, measures, par.set, ctrl,
    opt.path, show.info, xs, dobs = dob, eols = NA, remove.nas = remove.nas, resample.fun = resample.fun)
  ys = extractSubList(res.list, "y")
  # we return a numeric vec of y-values
  vnapply(ys, convertYForTuner, measures = measures, ctrl = ctrl)
}

# short helper that imputes illegal values and also negates for maximization problems
convertYForTuner = function(y, measures, ctrl, always.minimize = TRUE) {
  is.multicrit = inherits(ctrl, "TuneMultiCritControl")
  k = ifelse(is.multicrit, length(y), 1L)
  for (j in seq_len(k)) {
    z = y[[j]]
    # if there was any problem we return the imputed value that the user selected
    if (is.na(z) || is.nan(z) || is.infinite(z)) {
      z = ctrl$impute.val[[j]]
    }
    # we now negate values for maximization
    y[[j]] = if (always.minimize && !measures[[j]]$minimize) -1 * z else z
  }
  # for multicrit, return vector (without names), otherwise just scalar y
  if (inherits(ctrl, "TuneMultiCritControl")) {
    return(as.numeric(y))
  } else {
    return(y[[1L]])
  }
}
