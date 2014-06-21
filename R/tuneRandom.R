tuneRandom = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  vals = sampleValues(n = control$extra.args$maxit, par = par.set, trafo = TRUE)
  evalOptimizationStatesTune(learner, task, resampling, measures, par.set, control, opt.path,
    show.info, vals, dobs = seq_along(vals), eols = NA_integer_, remove.nas = TRUE)
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties = "random")
  e = getOptPathEl(opt.path, i)
  makeTuneResult(learner, control, removeMissingValues2(e$x), e$y, opt.path)
}


