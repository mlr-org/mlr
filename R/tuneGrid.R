# tunes with grid search, all params are supported as we use generateGridDesign
tuneGrid = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  des = generateGridDesign(par.set, resolution = control$extra.args$resolution, trafo = TRUE)
  xs = dfRowsToList(des, par.set)
  evalOptimizationStatesTune(learner, task, resampling, measures, par.set, control, opt.path,
    show.info, xs, dobs = seq_along(xs), eols = NA_integer_, remove.nas = FALSE)
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties = "random")
  e = getOptPathEl(opt.path, i)
  makeTuneResult(learner, control, e$x, e$y, opt.path)
}

