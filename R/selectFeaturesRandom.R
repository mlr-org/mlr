selectFeaturesRandom = function(learner, task, resampling, measures, bit.names, bits.to.features,
  control, opt.path, show.info) {

  states = lapply(seq_len(control$maxit), function(i) createStates(n = length(bit.names),
    max.features = control$max.features, prob = control$extra.args$prob))
  evalOptimizationStatesFeatSel(learner, task, resampling, measures, bits.to.features,
    control, opt.path, show.info, states, 1L, NA_integer_)
  makeFeatSelResultFromOptPath(learner, measures, control, opt.path)
}

# help function in order to respect max.features

createStates = function(n, max.features, prob){
  if (is.na(max.features))
    return(rbinom(n, 1, prob))
  run.loop = TRUE
  while (run.loop) {
    x = rbinom(n, 1, prob)
    if (sum(x) <= max.features)
      run.loop = FALSE
  }
  return(x)
}
