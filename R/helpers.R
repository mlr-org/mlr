#FIXME remove when PH get updated on cran
removeMissingValues2 = function(x) {
  Filter(Negate(isScalarNA), x)
}

requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}

measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep=".")
}

perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits=3L), sep=""), collapse=",")
}

recodeY = function(y, type, positive) {
  switch(type,
    "no" = y,
    "01" = as.numeric(y == positive),
    "-1+1" = as.numeric(2L*(y == positive)-1L),
    "surv" = Surv(time = y[, 1L], event = y[, 2L], type="right"),
    stop("Unknown value for 'type'"))
}

##### tuning #####
makeOptPathDFFromMeasures = function(par.set, measures) {
  ns = sapply(measures, measureAggrName)
  if (any(duplicated(ns)))
    stop("Cannot create OptPath, measures do not have unique ids!")
  if (length(intersect(ns, names(par.set$pars))) > 0 ||
    length(intersect(ns, getParamIds(par.set, repeated=TRUE, with.nr=TRUE))) > 0)
    stop("Cannot create OptPath, measures ids and dimension names of input space overlap!")
  minimize = sapply(measures, function(m) m$minimize)
  makeOptPathDF(par.set, ns, minimize, add.transformed.x=TRUE)
}


# evals a set of var-lists and return the corresponding states
logFunTune = function(learner, task, resampling, measures, par.set, control, opt.path, x, y, remove.nas) {
  i = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  messagef("[Tune] %i: %s : %s", i,
    paramValueToString(par.set, x, show.missing.values=!remove.nas), perfsToString(y))
}

removeFromDots = function(ns, ...) {
  args = list(...)
  args[setdiff(names(args), ns)]
}

##### featsel #####

logFunSelFeatures = function(learner, task, resampling, measures, par.set, control, opt.path, x, y, remove.nas) {
  i = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  messagef("[selectFeatures] %i: %i bits: %s", i, sum(x), perfsToString(y))
}

featuresToLogical = function(vars, all.vars) {
  if (is.list(vars)) {
    y = t(sapply(vars, function(x) all.vars %in% x))
    colnames(y) = all.vars
  } else {
    y = all.vars %in% vars
    names(y) = all.vars
  }
  y
}

featuresToBinary = function(vars, all.vars) {
  y=featuresToLogical(vars, all.vars)
  mode(y) = "integer"
  y
}

logicalToFeatures = function(x, all.vars) {
  if (is.matrix(x)) {
    if (missing(all.vars))
      all.vars = colnames(x)
    lapply(1:nrow(x), function(i) all.vars[x[i,]])
  } else {
    if (missing(all.vars))
      all.vars = names(x)
    all.vars[x]
  }
}

binaryToFeatures = function(x, all.vars) {
  mode(x) = "logical"
  logicalToFeatures(x, all.vars)
}

compare.diff = function(state1, state2, control, measure, threshold) {
  ifelse(measure$minimize, 1, -1) * (state1$y[1] - state2$y[1]) > threshold
}

filterNull = function(x) {
  x[!vapply(x, is.null, logical(1L))]
}

attachTrainingInfo = function(x, info) {
  attr(x, "mlr.train.info") = as.list(info)
  x
}

getTrainingInfo = function(x) {
  attr(x, "mlr.train.info")
}

#FIXME Put to BBmisc or abandon?
isNotSet = function(x) {
  missing(x) || is.null(x)
}
