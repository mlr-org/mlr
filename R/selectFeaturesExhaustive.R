selectFeaturesExhaustive = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {
  p = length(bit.names)
  states = list(rep(0, p))
  for (i in seq_len(min(control$max.features, p, na.rm = TRUE))) {
    x = combn(seq_len(p), i)
    s = lapply(seq_col(x), function(j) {
      b = rep(0, p)
      b[x[, j]] = 1
      b
    })
    states = c(states, s)
  }
  evalOptimizationStatesFeatSel(learner, task, resampling, measures, bits.to.features, control,
    opt.path, show.info, states, 1L, as.integer(NA))
  makeFeatSelResultFromOptPath(learner, measures, resampling, control, opt.path, task = task, bits.to.features = bits.to.features)
}
