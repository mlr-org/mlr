selectFeaturesGA = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {

  # generate mu feature sets (of correct size)
  states = list()
  mu = control$extra.args$mu
  lambda = control$extra.args$lambda
  yname = opt.path$y.names[1]
  minimize = opt.path$minimize[1]
  for (i in seq_len(mu)) {
    while (TRUE) {
      states[[i]] = rbinom(length(bit.names), 1, 0.5)
      if (is.na(control$max.features) || sum(states[[i]] <= control$max.features)) {
        break
      }
    }
  }
  evalOptimizationStatesFeatSel(learner, task, resampling, measures,
    bits.to.features, control, opt.path, show.info, states, 0L, NA_integer_)
  pop.inds = seq_len(mu)
  for (i in seq_len(control$maxit)) {
    # get all mu elements which are alive, ie the current pop and their bit vecs as matrix
    pop.df = as.data.frame(opt.path)[pop.inds, , drop = FALSE]
    pop.featmat = as.matrix(pop.df[, bit.names, drop = FALSE])
    mode(pop.featmat) = "integer"
    pop.y = pop.df[, yname]
    # create lambda offspring and eval
    kids.list = replicate(lambda, generateKid(pop.featmat, control), simplify = FALSE)
    kids.evals = evalOptimizationStatesFeatSel(learner, task, resampling, measures,
      bits.to.features, control, opt.path, show.info, states = kids.list, i, as.integer(NA))
    kids.y = extractSubList(kids.evals, c("y", yname))
    oplen = getOptPathLength(opt.path)
    kids.inds = seq(oplen - lambda + 1, oplen)
    if (control$extra.args$comma) {
      # if comma, kill current pop and keep only mu best of offspring
      setOptPathElEOL(opt.path, pop.inds, i - 1)
      pool.inds = kids.inds
      pool.y = kids.y
    } else {
      # if plus, keep best of pop + offspring
      pool.inds = c(pop.inds, kids.inds)
      pool.y = c(pop.y, kids.y)
    }
    # get next pop of best mu from pool
    pop.inds = pool.inds[order(pool.y, decreasing = !minimize)[seq_len(mu)]]
    setOptPathElEOL(opt.path, setdiff(pool.inds, pop.inds), i)
  }
  makeFeatSelResultFromOptPath(learner, measures, resampling, control, opt.path, task = task, bits.to.features = bits.to.features)
}


# sample 2 random parents, CX, mutate --> 1 kid
# (repeat in a loop if max.features not satisfied)
generateKid = function(featmat, control) {
  parents = sample(seq_row(featmat), 2L, replace = TRUE)
  while (TRUE) {
    kid = crossover(featmat[parents[1L], ], featmat[parents[2L], ], control$extra.args$crossover.rate)
    kid = mutateBits(kid, control$extra.args$mutation.rate)
    if (is.na(control$max.features) || sum(kid) <= control$max.features) {
      break
    }
  }
  return(kid)
}
