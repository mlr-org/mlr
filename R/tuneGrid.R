tuneGrid = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  # drop names from par.set
  vals = getValues(par.set)
  inds = lapply(vals, seq_along)
  grid = expand.grid(inds)
  vals = lapply(seq_len(nrow(grid)), function(i) {
    val.inds = as.numeric(grid[i,])
    Map(function(v, j) v[[j]], vals, val.inds)
  })
  evalOptimizationStatesTune(learner, task, resampling, measures, par.set, control, opt.path,
    show.info, vals, dobs = seq_along(vals), eols = NA_integer_, remove.nas = FALSE)

  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties = "random")
  e = getOptPathEl(opt.path, i)
  makeTuneResult(learner, control, e$x, e$y, opt.path)
}




