# tunes with a given data.frame conatining the design.
tuneDesign = function(learner, task, resampling, measures, par.set, control, opt.path, show.info, resample.fun) {
  xs = dfRowsToList(control$extra.args$design, par.set)
  evalOptimizationStatesTune(learner, task, resampling, measures, par.set, control, opt.path,
    show.info, xs, dobs = seq_along(xs), eols = NA_integer_, remove.nas = TRUE,
    resample.fun = resample.fun)
  makeTuneResultFromOptPath(learner, par.set, measures, resampling, control, opt.path)
}
