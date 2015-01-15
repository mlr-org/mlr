tuneMBO = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info) {

  # requirePackages("mlrMBO", why = "tuneMBO")
  mbo.control = control$mbo.control

  if (any(!mbo.control$minimize))
    stop("Maximization is handled by mlr, set all elements of 'minimize' to 'TRUE' in mbo.control.")

  # set final evals to 0 to save time. we dont really need final evals in this context.
  mbo.control$final.evals = 0L
  cx = identity

  tff = function(x) tunerFitnFun(x, learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
    convertx = cx, remove.nas = TRUE)

  # remove trafos for mbo, we do this in tunerFitnFun
  ps2 = par.set
  for (i in seq_along(ps2$pars))
    ps2$pars[[i]]$trafo = NULL

  state = mbo.control$save.file.path
  if (control$continue && file.exists(state)) {
    messagef("Resuming previous MBO run using state in '%s'...", state)
    # FIXME: remove this when mbo on cran
    mbofun = get("mboContinue", envir = getNamespace("mlrMBO"))
    or = mbofun(state)
  } else {
    # FIXME: remove this when mbo on cran
    mbofun = get("mbo", envir = getNamespace("mlrMBO"))
    or = mbofun(tff, ps2, design = NULL, learner = control$learner, control = mbo.control, show.info = FALSE)
  }

  x = trafoValue(par.set, or$x)
  # we take the point that mbo proposes and its estimated y
  makeTuneResult(learner, control, x, setNames(or$y, opt.path$y.names[1L]), opt.path)
}
