tuneIrace = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {

  requirePackages("irace", why = "tuneIrace")

  #FIXME allow to do in parallel
  hookRun = function(instance, candidate, extra.params = NULL, config = list()) {
    rin = instance
    tunerFitnFun(candidate$values, learner = learner, task = task, resampling = rin, measures = measures,
      par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
      trafo = TRUE, convertx = identity, remove.nas = TRUE)
  }
  n.instances = control$extra.args$n.instances
  control$extra.args$n.instances = NULL
  show.irace.output = control$extra.args$show.irace.output
  control$extra.args$show.irace.output = NULL
  instances = lapply(1:n.instances, function(i) makeResampleInstance(resampling, task = task))

  parameters = convertParamSetToIrace(par.set)
  log.file = tempfile()
  tuner.config = c(list(hookRun = hookRun, instances = instances, logFile = log.file), control$extra.args)

  g = if (show.irace.output) identity else capture.output
  g(or <- irace(tunerConfig = tuner.config, parameters = parameters))
  unlink(log.file)
  if (nrow(or) == 0)
    stop("irace produced no result, possibly the budget was set too low?")
  id = or[1,1]
  # get best candidate
  x = as.list(removeCandidatesMetaData(or[1,]))
  x = trafoValue(par.set, x)
  d = as.data.frame(opt.path)
  par.names = names(x)
  # get all lines in opt.path which correspond to x and average their perf values
  j = sapply(1:nrow(d), function(i) isTRUE(all.equal(as.list(d[i, par.names, drop = FALSE]), x)))
  if (!any(j))
    stop("No matching rows for final elite candidate found in opt.path! This cannot be!")
  y = colMeans(d[j, opt.path$y.names, drop = FALSE])
  makeTuneResult(learner, control, removeMissingValues(x), y, opt.path)
}
