selectFeaturesExhaustive = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {
  p = length(bit.names)
  states = list(rep(0, p))
  for (i in 1:min(control$max.features, p, na.rm=TRUE)) {
    x = combn(1:p, i)
    s = lapply(1:ncol(x), function(j) { 
        b = rep(0, p)
        b[x[,j]] = 1
        b
    })
    states = c(states, s)
  }
  evalOptimizationStatesFeatSel(learner, task, resampling, measures, bits.to.features, control, opt.path, show.info, states, 1L, as.integer(NA))
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
	makeFeatSelResult(learner, control, names(e$x)[e$x == 1], e$y, opt.path)
} 
