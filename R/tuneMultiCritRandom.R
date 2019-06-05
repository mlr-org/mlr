tuneMultiCritRandom = function(learner, task, resampling, measures, par.set, control, opt.path, show.info, resample.fun) {
  vals = sampleValues(n = control$extra.args$maxit, par = par.set, trafo = FALSE)
  evalOptimizationStatesTune(learner, task, resampling, measures, par.set, control, opt.path,
    show.info, vals, dobs = seq_along(vals), eols = NA_integer_, remove.nas = TRUE,
    resample.fun = resample.fun)
  makeTuneMultiCritResultFromOptPath(learner, par.set, measures, resampling, control, opt.path)
}
