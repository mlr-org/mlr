tuneMBO = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info, resample.fun) {

  requirePackages("mlrMBO", why = "tuneMBO", default.method = "load")
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

  if (!is.null(mbo.control$n.objectives)) {
    assertList(measures, len = mbo.control$n.objectives)
  }
  multicrit = !is.null(mbo.control$n.objectives) && mbo.control$n.objectives > 1L

  tff = tunerSmoofFun(learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = convertXIdentity, remove.nas = TRUE, resample.fun = resample.fun)

  state = mbo.control$save.file.path
  if (control$continue && file.exists(state)) {
    messagef("Resuming previous MBO run using state in '%s'...", state)
    or = mlrMBO::mboContinue(state)
  } else {
    or = mlrMBO::mbo(tff, design = control$mbo.design, learner = control$learner,
      control = mbo.control, show.info = FALSE)
  }

  if (multicrit) {
    df = convertListOfRowsToDataFrame(or$pareto.set)
    x = lapply(seq_len(nrow(df)), function(i) trafoValue(par.set, as.list(df[i, ])))
    y = or$pareto.front
    colnames(y) = opt.path$y.names[1:mbo.control$n.objectives]
    ind = or$pareto.inds
  } else {
    x = trafoValue(par.set, or$x)
    y = setNames(or$y, opt.path$y.names[1L])
    # we take the point that mbo proposes and its estimated y
  }

  # FIXME: threshold
  if (!control$mbo.keep.result)
    or = NULL

  if (multicrit) {
    res = makeTuneMultiCritResult(learner, ind, removeMissingValues(x), y, control,
      opt.path, measures, mbo.result = or)
  } else {
    res = makeTuneResult(learner, control, removeMissingValues(x), y, NULL,
      opt.path, mbo.result = or)
  }

  return(res)
}
