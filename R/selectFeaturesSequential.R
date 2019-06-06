# FIXME: compare relative
selectFeaturesSequential = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {

  seq.step = function(forward, state, gen.new.states, compare) {

    # we have too many vars already and cannot move forward
    if (forward && !is.na(control$max.features) && control$max.features <= sum(unlist(state$x))) {
      return(NULL)
    }
    xs = gen.new.states(state$x)
    if (length(xs) == 0) {
      return(NULL)
    }
    dob = max(opt.path$env$dob) + 1L
    # die at once
    evalOptimizationStatesFeatSel(learner, task, resampling, measures, bits.to.features, control, opt.path, show.info, xs, dob, dob)

    best.i = getOptPathBestIndex(opt.path, dob = dob, ties = "random")
    best = getOptPathEl(opt.path, best.i)
    # best element lives one iteration longer
    thresh = ifelse(forward, control$extra.args$alpha, control$extra.args$beta)
    better = compare(state, best, control, measures[[1]], thresh)
    # if backward step and we have too many vars we do always go to the next best state with one less var.
    if ((forward && better) || (!forward && (better || (!is.na(control$max.features) && sum(unlist(state$x)) > control$max.features)))) {
      setOptPathElEOL(opt.path, best.i, dob + 1)
      return(best)
    } else {
      return(NULL)
    }
  }

  gen.new.states.sfs = function(x) {
    xs = list()
    for (i in seq_along(x)) {
      if (x[i] == 0) {
        y = x
        y[i] = 1
        xs[[length(xs) + 1L]] = y
      }
    }
    xs
  }

  gen.new.states.sbs = function(x) {
    xs = list()
    for (i in seq_along(x)) {
      if (x[i] == 1) {
        y = x
        y[i] = 0
        xs[[length(xs) + 1L]] = y
      }
    }
    xs
  }

  dim = length(bit.names)
  compare = compare.diff
  method = control$extra.args$method

  x = switch(method,
    sfs = rep(0, dim),
    sbs = rep(1, dim),
    sffs = rep(0, dim),
    sfbs = rep(1, dim),
    stop(stri_paste("Unknown method:", method, sep = " "))
  )

  gen.new.states = switch(method,
    sfs = gen.new.states.sfs,
    sbs = gen.new.states.sbs,
    sffs = gen.new.states.sfs,
    sfbs = gen.new.states.sbs,
    stop(stri_paste("Unknown method:", method, sep = " "))
  )
  res = evalOptimizationState(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, show.info, 1L, x, FALSE, resample)
  # add stuff to opt.path
  state = list(x = x, y = res$y)
  extra = getTuneThresholdExtra(control, res)
  addOptPathEl(opt.path, x = as.list(x), y = res$y, dob = 1L, eol = 2L, exec.time = res$exec.time, error.message = res$errmsg, extra = extra)

  forward = (method %in% c("sfs", "sffs"))
  fail = 0
  while ((method %in% c("sfs", "sbs") && fail == 0) || (method %in% c("sffs", "sfbs") && fail < 2)) {
    state2 = seq.step(forward, state, gen.new.states, compare)
    # we could not move to state2 in normal step, stay where we are
    if (!is.null(state2)) {
      state = state2
      state$x = unlist(state$x)
      fail = 0
    } else {
      fail = fail + 1
    }
    if (method %in% c("sffs", "sfbs")) {
      # cat("forward:", !forward, "\n")
      gns = switch(method,
        sffs = gen.new.states.sbs,
        sfbs = gen.new.states.sfs
      )
      state2 = seq.step(!forward, state, gns, compare)
      if (!is.null(state2)) {
        state = state2
        state$x = unlist(state$x)
        fail = 0
      } else {
        fail = fail + 1
      }
    }
  }

  # if last generation contains no better element, go to second to last
  last = max(opt.path$env$dob)

  if (all(opt.path$env$eol[opt.path$env$dob == last] == last)) {
    last = last - 1
  }
  makeFeatSelResultFromOptPath(learner, measures, resampling, control, opt.path, dob = last, ties = "first", task = task, bits.to.features = bits.to.features)
}
