selectFeaturesRandom = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {
  states = lapply(1:control$maxit, function(i) createStates(n = length(bit.names), max.features = control$max.features, prob = control$extra.args$prob))
  evalOptimizationStates(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, show.info, states, 1L, as.integer(NA), FALSE)
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
	makeFeatSelResult(learner, control, names(e$x)[e$x == 1], e$y, opt.path)
} 

## help function in order to respect max.features
createStates = function(n, max.features, prob){
  if(is.na(max.features)) return(rbinom(n, 1, prob) )
  run.loop = TRUE
  while(run.loop){
    x = rbinom(n, 1, prob) 
    if(sum(x) <= max.features) run.loop = FALSE
  }
  return(x)
}