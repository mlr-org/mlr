# set default value fro y-imputation in optimization
setDefaultImputeVal = function(control, measures) {
  # if single crit we want a scalar impute val
  ms = if (inherits(control, "TuneMultiCritControl")) measures else measures[1L]
  getDefVal = function(mm) {
    if (identical(mm$aggr, test.mean) && is.finite(mm$worst))
      ifelse(mm$minimize, 1, -1) * mm$worst
    else
      Inf
  }
  if (is.null(control$impute.val))
    control$impute.val = vnapply(measures, getDefVal)
  return(control)
}


##### tuning #####
makeOptPathDFFromMeasures = function(par.set, measures) {
  ns = vcapply(measures, measureAggrName)
  if (anyDuplicated(ns))
    stop("Cannot create OptPath, measures do not have unique ids!")
  if (length(intersect(ns, names(par.set$pars))) > 0L ||
    length(intersect(ns, getParamIds(par.set, repeated = TRUE, with.nr = TRUE))) > 0L)
    stop("Cannot create OptPath, measures ids and dimension names of input space overlap!")
  minimize = vlapply(measures, function(m) m$minimize)
  makeOptPathDF(par.set, ns, minimize, add.transformed.x = FALSE,
    include.error.message = TRUE, include.exec.time = TRUE)
}


# evals a set of var-lists and return the corresponding states
logFunTune = function(learner, task, resampling, measures, par.set, control, opt.path, dob, x, y, remove.nas) {
  if (!inherits(learner, "ModelMultiplexer")) {
    messagef("[Tune] %i: %s : %s", dob,
      paramValueToString(par.set, x, show.missing.values = !remove.nas), perfsToString(y))
  } else {
    # shorten tuning logging a bit. we remove the sel.learner prefix from params
    s = paramValueToString(par.set, x, show.missing.values = !remove.nas)
    s = gsub(paste0(x$selected.learner, "\\."), "", s)
    messagef("[Tune] %i: %s : %s", dob, s, perfsToString(y))
  }
}

##### featsel #####

logFunSelFeatures = function(learner, task, resampling, measures, par.set, control, opt.path, dob, x, y, remove.nas) {
  messagef("[FeatSel] %i: %i bits: %s", dob, sum(x), perfsToString(y))
}

featuresToLogical = function(vars, all.vars) {
  if (is.list(vars)) {
    # FIXME: use asMatrixCols / asMatrixRows
    y = t(sapply(vars, function(x) all.vars %in% x))
    colnames(y) = all.vars
  } else {
    y = all.vars %in% vars
    names(y) = all.vars
  }
  y
}

featuresToBinary = function(vars, all.vars) {
  y = featuresToLogical(vars, all.vars)
  mode(y) = "integer"
  y
}

logicalToFeatures = function(x, all.vars) {
  if (is.matrix(x)) {
    if (missing(all.vars))
      all.vars = colnames(x)
    lapply(seq_row(x), function(i) all.vars[x[i,]])
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

# FIXME: remove this when PH is fixed on CRAN
addOptPathElFixed = function(op, x, y, dob = getOptPathLength(op) + 1L, eol = as.integer(NA),
    error.message = NA_character_, exec.time = NA_real_, extra = NULL,
    check.feasible = !op$add.transformed.x) {

  addOptPathEl(op = op, x = x, y = y, dob = dob, eol = eol, error.message = error.message,
    exec.time = exec.time, extra = extra, check.feasible = check.feasible)
  types = getParamTypes(op$par.set, df.cols = TRUE, df.discretes.as.factor = FALSE)
  # print(types)
  for (j in seq_along(types)) {
    type = types[j]
    g = get(sprintf("as.%s", type))
    op$env$path[, j] = g(op$env$path[, j])
  }
}

