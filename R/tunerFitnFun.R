# generates an R function that we can pass to a tuner to optrimize hyper pars
# - possibly convert x with custom functon
# - possibly trafo x
# - eval states (maybe in parallel)
# - add evals to opt path
# - return y scalar (vector for vectorized version below), always minimized

# one x
tunerFitnFun = function(x, learner, task, resampling, measures, par.set, ctrl,
  opt.path, show.info, trafo, convertx, remove.nas) {

  x = convertx(x)
  x = mytrafo(trafo, par.set, x)
  # transform parameters
  dob = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  y = evalOptimizationState(learner, task, resampling, measures, par.set, NULL, ctrl,
    opt.path, show.info, dob, x, remove.nas)
  addOptPathEl(opt.path, x = x, y = y, dob = dob, eol = NA, check.feasible = FALSE)
  #returns resample$aggr vector, take 1st
  #FIXME: what should happen if we get an infeasible y here? docu this!
  ifelse(measures[[1]]$minimize, 1 , -1) * y[[1]]
}

# multiple xs in parallel
tunerFitnFunVectorized = function(xs, learner, task, resampling, measures, par.set, ctrl,
  opt.path, show.info, trafo, convertx, remove.nas) {

  xs = convertx(xs)
  # transform parameters
  xs = lapply(xs, mytrafo, par.set = par.set, trafo = trafo)
  dob = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  ys = evalOptimizationStatesTune(learner, task, resampling, measures, par.set, ctrl,
    opt.path, show.info, xs, dobs = dob, eols = NA, remove.nas = remove.nas)
  #returns list of resample$aggr vectors, take 1st
  ys = sapply(ys, function(a) a[[1]])
  ifelse(measures[[1]]$minimize, 1 , -1) * ys
}

# short helper to trafo x depending on boolean flag, only used in this file
mytrafo = function(trafo, par.set, x) {
  if (trafo)
    trafoValue(par.set, x)
  else
    x
}
