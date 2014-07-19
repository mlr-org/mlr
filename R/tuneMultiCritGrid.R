# tunes with grid search, all params are supported as we use generateGridDesign
tuneMultiCritGrid = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  des = generateGridDesign(par.set, resolution = control$extra.args$resolution, trafo = FALSE)
  xs = dfRowsToList(des, par.set)
  evalOptimizationStatesTune(learner, task, resampling, measures, par.set, control, opt.path,
    show.info, xs, dobs = seq_along(xs), eols = NA_integer_, remove.nas = TRUE)
  makeTuneMultiCritResultFromOptPath(learner, par.set, measures, control, opt.path)
}


