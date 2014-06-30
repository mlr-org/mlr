# evaluates either a hyperpar setting or a feature set by resampling
# must be already in correct format, either a named list of values or a named integer vector for features
# logs point and results
# return y-value(s), exectime, and potential erorr msg
evalOptimizationState = function(learner, task, resampling, measures, par.set, bits.to.features, control,
  opt.path, show.info, dob, state, remove.nas) {

  y = setNames(rep(NA_real_, length(measures)), sapply(measures, measureAggrName))
  errmsg = NA_character_
  exec.time = NA_real_
  evalok = TRUE
  learner2 = learner
  if (inherits(control, "TuneControl")) {
    log.fun = logFunTune
    state2 = if (remove.nas) removeMissingValues(state) else state
    learner2 = try(setHyperPars(learner, par.vals = state2))
    # if somebody above (eg tuner) prodcued bad settings, we catch this here and dont eval
    if (is.error(learner2)) {
      evalok = FALSE
      errmsg = as.character(learner2)
    }
  } else if (inherits(control, "FeatSelControl")) {
    log.fun = logFunSelFeatures
    task = subsetTask(task, features = bits.to.features(state, task))
  }

  # if no problems: resample + measure time
  if (evalok) {
    exec.time = system.time({
      r = resample(learner2, task, resampling, measures = measures, show.info = FALSE)
    })
    y = r$aggr
    exec.time = exec.time[3L]
  }
  # if eval was not ok, everything should have been initailized to NAs

  if (show.info)
    log.fun(learner, task, resampling, measures, par.set, control, opt.path, dob, state, y, remove.nas)
  list(y = y, exec.time = exec.time, errmsg = errmsg)
}

# evaluates a list of states by calling evalOptimizationState
# must be already in correct format, either a named list of values or a named integer vector for features
# might be done in parallel
# logs point and results
# adds points to path
# returns list of lists, the single eval results
evalOptimizationStates = function(learner, task, resampling, measures, par.set, bits.to.features, control,
  opt.path, show.info, states, dobs, eols, remove.nas, level) {

  n = length(states)
  if (length(dobs) == 1L)
    dobs = rep(dobs, n)
  if (length(eols) == 1L)
    eols = rep(eols, n)
  parallelLibrary("mlr", master = FALSE, level = level, show.info = FALSE)
  exportMlrOptions()
  res.list = parallelMap(evalOptimizationState, dobs, states, level = level,
    more.args = list(learner = learner, task = task, resampling = resampling,
      measures = measures, par.set = par.set, bits.to.features = bits.to.features,
      control = control, opt.path = opt.path, show.info = show.info, remove.nas = remove.nas))
  # add stuff to opt.path
  for (i in seq_len(n)) {
    res = res.list[[i]]
    addOptPathEl(opt.path, x = as.list(states[[i]]), y = res$y, exec.time = res$exec.time,
      error.message = res$errmsg, dob = dobs[i], eol = eols[i], check.feasible = FALSE)
  }
  return(res.list)
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
