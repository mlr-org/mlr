tuneMultiCritMBO = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info, resample.fun) {

  requirePackages("mlrMBO", why = "tuneMultiCritMBO", default.method = "load")
  mbo.control = control$mbo.control

  # put all required info into the function env
  force(learner)
  force(task)
  force(resampling)
  force(measures)
  force(par.set)
  force(control)
  force(opt.path)
  force(show.info)

  assertList(measures, len = mbo.control$n.objectives)

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

  df = convertListOfRowsToDataFrame(or$pareto.set)
  x = lapply(1:nrow(df), function(i) trafoValue(par.set, as.list(df[i, ])))

  y = or$pareto.front
  colnames(y) = opt.path$y.names[1L:mbo.control$n.objectives]
  ind = or$pareto.inds

  # FIXME: threshold
  if (!control$mbo.keep.result)
    or = NULL
  res = makeTuneMultiCritResult(learner, ind, removeMissingValues(x), y, control, opt.path, measures, mbo.result = or)
  res
}
