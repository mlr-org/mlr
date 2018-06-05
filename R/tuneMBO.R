tuneMBO = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info, resample.fun) {

  requirePackages("mlrMBO", why = "tuneMBO", default.method = "load")
  mbo.control = control$mbo.control

  multicrit = mbo.control$n.objectives > 1L
  if (multicrit) {
    assertList(measures, len = mbo.control$n.objectives)
  }

  tff = tunerSmoofFun(learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = convertXIdentity, remove.nas = TRUE, resample.fun = resample.fun)

  state = mbo.control$save.file.path
  if (control$continue && file.exists(state)) {
    messagef("Resuming previous MBO run using state in '%s'...", state)
    or = mlrMBO::mboContinue(state)
  } else {
    or = mlrMBO::mbo(tff, design = control$mbo.design, learner = control$learner, control = mbo.control, show.info = FALSE)
  }

  if (multicrit) {
    x = lapply(or$pareto.set, function(z) trafoValue(par.set, z))
    y = or$pareto.front
    colnames(y) = opt.path$y.names
    ind = or$pareto.inds
  } else {
    # we take the point that mbo proposes and its estimated y
    x = trafoValue(par.set, or$x)
    y = setNames(or$y, opt.path$y.names[1L])
  }

  # if threshold tuning is on, we extract the threshold from extras
  if (control$tune.threshold) {
    el = getOptPathEl(opt.path, or$best.ind)
    th = el$extra$threshold
  } else {
    th = NULL
  }
  if (multicrit) {
    res = makeTuneMultiCritResult(learner, ind, removeMissingValues(x), y, resampling, control,
      opt.path, measures, mbo.result = or)
  } else {
    res = makeTuneResult(learner, control, removeMissingValues(x), y, resampling, th, opt.path,
      mbo.result = or)
  }

  return(res)
}
