#FIXME: read all 
# evaluates either a hyperpar setting or a feature set by resampling
# must be already in correct format, either a named list of values or a named integer vector for features
# logs point and results
evalOptimizationState = function(learner, task, resampling, measures, par.set, bits.to.features, control,
  opt.path, show.info, state, remove.nas, mlr.options=NULL) {
  
  #FIXME bad
  if (isTRUE(getOption("parallelMap.on.slave"))) {
    do.call(options, mlr.options)
  }
  
  if (inherits(control, "TuneControl")) {
    # FIXME change when new version of paramhelpers is online
    state2 = if (remove.nas) ParamHelpers:::removeMissingValues(state) else state
    learner = try(setHyperPars(learner, par.vals=state2), silent=TRUE)
    log.fun = logFunTune
  } else  if (inherits(control, "FeatSelControl")) {
    task = subsetTask(task, features=bits.to.features(state, task))
    log.fun = logFunSelFeatures
  }
  
  # FIXME: reflect? just returning +- inf is not good!
  # params became infeasible when we could not model constraints
  # also error msg is annoying
  if (is.error(learner)) { 
    if (grep("not a feasible parameter setting", learner) > 0)
      y = ifelse(measures[[1]]$minimize, 1 , -1) * Inf
    else
      stop(learner)
  } else {
    r = resample(learner, task, resampling, measures=measures, show.info=FALSE)
    y = r$aggr
  }
  if (show.info)
    log.fun(learner, task, resampling, measures, par.set, control, opt.path, state, y, remove.nas)
  return(y)
}

# evaluates a list of states by calling evalOptimizationState
# must be already in correct format, either a named list of values or a named integer vector for features
# might be done in parallel
# logs point and results
# adds points to path
evalOptimizationStates = function(learner, task, resampling, measures, par.set, bits.to.features, control,
  opt.path, show.info, states, dobs, eols, remove.nas) {

  n = length(states)
  if (length(dobs) == 1)
    dobs = rep(dobs, n)
  if (length(eols) == 1)
    eols = rep(eols, n)
  #FIXME this is bad, remove this soon
  mlr.options = options("mlr.on.learner.error", "mlr.on.par.without.desc", "mlr.show.learner.output")
  #parallelLibrary("mlr", level="mlr.tune", master=FALSE)
  # FIXME better export
  ys = parallelMap(evalOptimizationState, states, level="mlr.tune", 
    more.args=list(learner=learner, task=task, resampling=resampling,
      measures=measures, par.set=par.set, bits.to.features=bits.to.features, 
      control=control, opt.path=opt.path, show.info=show.info, remove.nas=remove.nas,
      mlr.options=mlr.options))
  # add stuff to opt.path
  for (i in seq_len(n)) 
    addOptPathEl(opt.path, x=as.list(states[[i]]), y=ys[[i]], 
      dob=dobs[i], eol=eols[i], check.feasible=FALSE)
  return(ys)  
}
