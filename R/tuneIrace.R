tuneIrace = function(learner, task, resampling, measures, par.set, control, opt.path, show.info, resample.fun) {

  requirePackages("irace", why = "tuneIrace", default.method = "load")

  targetRunnerParallel = function(experiment, exec.target.runner, scenario, target.runner) {

    # get our param settings that irace should try
    cands = extractSubList(experiment, "configuration", simplify = FALSE)
    # some conversion code
    cands = lapply(cands, as.data.frame)
    cands = lapply(cands, dfRowToList, i = 1, par.set = par.set,
      enforce.col.types = TRUE)

    # the instance is always the same for all different param setting
    rin = experiment[[1L]]$instance

    ys = tunerFitnFunVectorized(cands, learner = learner, task = task, resampling = rin, measures = measures,
      par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
      convertx = convertXVectorizedBooleanStringsToLogical, remove.nas = TRUE, resample.fun)
    # FIXME: irace can also use time now, we should add it
    res = lapply(ys, function(y) list(cost = y, time = NA_real_))
    return(res)
  }

  n.instances = control$extra.args$n.instances
  control$extra.args$n.instances = NULL
  show.irace.output = control$extra.args$show.irace.output
  control$extra.args$show.irace.output = NULL
  instances = lapply(seq_len(n.instances), function(i) makeResampleInstance(resampling, task = task))
  if (is.null(control$extra.args$digits)) {
    control$extra.args$digits = 15
  } else {
    control$extra.args$digits = asInt(control$extra.args$digits)
  }

  parameters = convertParamSetToIrace(par.set)
  log.file = tempfile()
  tuner.config = c(list(targetRunnerParallel = targetRunnerParallel,
    instances = instances, logFile = log.file), control$extra.args)
  g = if (show.irace.output) identity else capture.output
  g({
    or = irace::irace(scenario = tuner.config, parameters = parameters)
  })
  unlink(log.file)
  if (nrow(or) == 0L) {
    stop("irace produced no result, possibly the budget was set too low?")
  }
  # get best configuarion
  x1 = as.list(irace::removeConfigurationsMetaData(or[1L, ]))
  # we need chars, not factors / logicals, so we can match 'x'
  d = convertDfCols(as.data.frame(opt.path), logicals.as.factor = TRUE)
  d = convertDfCols(d, factors.as.char = TRUE)
  par.names = names(x1)
  # get all lines in opt.path which correspond to x and average their perf values
  j = vlapply(seq_row(d), function(i) {
    isTRUE(all.equal(removeMissingValues(as.list(d[i, par.names, drop = FALSE])),
      removeMissingValues(x1)))
  })
  if (!any(j)) {
    stop("No matching rows for final elite configuarion found in opt.path! This cannot be!")
  }
  y = colMeans(d[j, opt.path$y.names, drop = FALSE])
  # take first index of mating lines to get recommended x
  e = getOptPathEl(opt.path, which.first(j))
  x = trafoValue(par.set, e$x)
  x = removeMissingValues(x)
  if (control$tune.threshold) {
    # now get thresholds and average them
    threshold = getThresholdFromOptPath(opt.path, which(j))
  } else {
    threshold = NULL
  }
  makeTuneResult(learner, control, x, y, resampling, threshold, opt.path)
}
