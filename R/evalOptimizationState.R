#FIXME: read all
# evaluates either a hyperpar setting or a feature set by resampling
# must be already in correct format, either a named list of values or a named integer vector for features
# logs point and results
evalOptimizationState = function(learner, task, resampling, measures, par.set, bits.to.features, control,
  opt.path, show.info, dob, state, remove.nas) {

  if (inherits(control, "TuneControl")) {
    # FIXME: change when new version of paramhelpers is online
    state2 = if (remove.nas) removeMissingValues2(state) else state
    learner = try(setHyperPars(learner, par.vals = state2), silent = TRUE)
    log.fun = logFunTune
  } else  if (inherits(control, "FeatSelControl")) {
    task = subsetTask(task, features = bits.to.features(state, task))
    log.fun = logFunSelFeatures
  }

  # FIXME: reflect? just returning +- inf is not good!
  # params became infeasible when we could not model constraints
  # also error msg is annoying
  if (is.error(learner)) {
    msg = as.character(learner)
    if (length(grep("not feasible for parameter", msg)) > 0L)
      y = ifelse(measures[[1L]]$minimize, 1 , -1) * Inf
    else
      stop(msg)
  } else {
    r = resample(learner, task, resampling, measures = measures, show.info = FALSE)
    y = r$aggr
  }
  if (show.info)
    log.fun(learner, task, resampling, measures, par.set, control, opt.path, dob, state, y, remove.nas)
  return(y)
}

# evaluates a list of states by calling evalOptimizationState
# must be already in correct format, either a named list of values or a named integer vector for features
# might be done in parallel
# logs point and results
# adds points to path
evalOptimizationStates = function(learner, task, resampling, measures, par.set, bits.to.features, control,
  opt.path, show.info, states, dobs, eols, remove.nas, level) {

  n = length(states)
  if (length(dobs) == 1L)
    dobs = rep(dobs, n)
  if (length(eols) == 1L)
    eols = rep(eols, n)
  parallelLibrary("mlr", master = FALSE, level = level, show.info = FALSE)
  exportMlrOptions()
  ys = parallelMap(evalOptimizationState, dobs, states, level = level,
    more.args = list(learner = learner, task = task, resampling = resampling,
      measures = measures, par.set = par.set, bits.to.features = bits.to.features,
      control = control, opt.path = opt.path, show.info = show.info, remove.nas = remove.nas))
  # add stuff to opt.path
  for (i in seq_len(n))
    addOptPathEl(opt.path, x = as.list(states[[i]]), y = ys[[i]],
      dob = dobs[i], eol = eols[i], check.feasible = FALSE)
  return(ys)
}

evalOptimizationStatesTune = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info, states, dobs, eols, remove.nas) {

  evalOptimizationStates(learner, task, resampling, measures, par.set, NULL, control,
    opt.path, show.info, states, dobs, eols, remove.nas, "mlr.tuneParams")
}

evalOptimizationStatesFeatSel = function(learner, task, resampling, measures, bits.to.features, control,
  opt.path, show.info, states, dobs, eols) {

  evalOptimizationStates(learner, task, resampling, measures, NULL, bits.to.features, control,
    opt.path, show.info, states, dobs, eols, FALSE, "mlr.selectFeatures")
}
