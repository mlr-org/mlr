tuneRandom = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  vals = sampleValues(n=control$extra.args$maxit, par=par.set)
  evalOptimizationStates(learner, task, resampling, measures, par.set, NULL, control, opt.path, 
    show.info, vals, dobs=1L, eols=1L, remove.nas=TRUE)
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  # FIXME change when new version of paramhelpers is online
  # FIXME remove NAs always ok?
  makeTuneResult(learner, control, ParamHelpers:::removeMissingValues(e$x), e$y, opt.path)
}


