# generates an R function that we can pass to a tuner to optrimize hyper pars
# - possibly convert x with custom functon
# - eval states (maybe in parallel)
# - add evals to opt path
# - return y scalar (vector for vectorized version below), always minimized

# one x
tunerFitnFun = function(x, learner, task, resampling, measures, par.set, ctrl,
  opt.path, show.info, convertx, remove.nas) {

  x = convertx(x)
  # transform parameters
  dob = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  res = evalOptimizationState(learner, task, resampling, measures, par.set, NULL, ctrl,
    opt.path, show.info, dob, x, remove.nas)
  addOptPathEl(opt.path, x = x, y = res$y, dob = dob, eol = NA, check.feasible = TRUE,
    exec.time = res$exec.time, error.message = res$errmsg)
  convertYForTuner(res$y, measures, ctrl)
}

# multiple xs in parallel
tunerFitnFunVectorized = function(xs, learner, task, resampling, measures, par.set, ctrl,
  opt.path, show.info, convertx, remove.nas) {

  xs = convertx(xs)
  dob = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  res.list = evalOptimizationStatesTune(learner, task, resampling, measures, par.set, ctrl,
    opt.path, show.info, xs, dobs = dob, eols = NA, remove.nas = remove.nas)
  ys = extractSubList(res.list, "y")
  # we return a numeric vec of y-values
  # FIXME: convertYForTuner can return vectors! do not use sapply!
  sapply(ys, convertYForTuner, measures = measures, ctrl = ctrl)
}

# short helper that imputes illegal values and also negates for maximization problems
convertYForTuner = function(y, measures, ctrl) {
  is.multicrit = inherits(ctrl, "TuneMultiCritControl")
  k = ifelse(is.multicrit, length(y), 1L)
  for (j in seq_len(k)) {
    z = y[[j]]
    # if there was any problem we return the imputed value that the user selected
    if (is.na(z) || is.nan(z) || is.infinite(z))
      z = ctrl$impute.val[[j]]
    # we now negate values for maximization
    y[[j]] = z * ifelse(measures[[j]]$minimize, 1 , -1)
  }
  # for multicrit, return vector (without names), otherwise just scalar y
  if (inherits(ctrl, "TuneMultiCritControl"))
    return(as.numeric(y))
  else
    return(y[[1L]])
}
