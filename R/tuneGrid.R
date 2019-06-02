# tunes with grid search, all params are supported as we use generateGridDesign
tuneGrid = function(learner, task, resampling, measures, par.set, control, opt.path, show.info, resample.fun) {
  des = generateGridDesign(par.set, resolution = control$extra.args$resolution, trafo = FALSE)
  if (!is.null(control$budget) && (nrow(des) != control$budget)) {
    stopf("The given budget (%i) does not fit to the size of the grid (%i).", control$budget, nrow(des))
  }
  xs = dfRowsToList(des, par.set)
  evalOptimizationStatesTune(learner, task, resampling, measures, par.set, control, opt.path,
    show.info, xs, dobs = seq_along(xs), eols = NA_integer_, remove.nas = TRUE, resample.fun = resample.fun)
  makeTuneResultFromOptPath(learner, par.set, measures, resampling, control, opt.path)
}
