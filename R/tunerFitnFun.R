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
  sapply(ys, convertYForTuner, measures = measures, ctrl = ctrl)
}

# short helper that imputes illegal values and also negates for maximization problems
convertYForTuner = function(y, measures, ctrl) {
  # can be a vector
  y = y[[1L]]
  # if there was any problem we return the imputed value that the user selected
  if (is.na(y) || is.nan(y) || is.infinite(y))
    y = ctrl$impute.val
  # we now negate values for maximization
  y = y * ifelse(measures[[1]]$minimize, 1 , -1)
  return(y)
}
