# set default value fro y-imputation in optimization
setDefaultImputeVal = function(control, measures) {
  getDefVal = function(mm) {
    if (identical(mm$aggr, test.mean) && is.finite(mm$worst)) {
      ifelse(mm$minimize, 1, -1) * mm$worst
    } else {
      Inf
    }
  }
  if (is.null(control$impute.val)) {
    control$impute.val = vnapply(measures, getDefVal)
  }
  return(control)
}

# get one or multiple thresholds vector from optpath rows
# if we have multiple rows we average the result
# subset to those elements, which begin with "threshold." and also remove that prefix
getThresholdFromOptPath = function(opt.path, inds) {
  ths = asMatrixCols(lapply(inds, function(i) {
    ex = getOptPathEl(opt.path, i)$extra
    ns = names(ex)
    ex = ex[stri_detect_regex(ns, "^threshold")]
    setNames(ex, stri_replace_first(names(ex), "", regex = "^threshold\\."))
  }))
  rowMeans(ths)
}

##### tuning #####
makeOptPathDFFromMeasures = function(par.set, measures, ...) {
  ns = vcapply(measures, measureAggrName)
  if (anyDuplicated(ns)) {
    stop("Cannot create OptPath, measures do not have unique ids!")
  }
  if (length(intersect(ns, names(par.set$pars))) > 0L ||
    length(intersect(ns, getParamIds(par.set, repeated = TRUE, with.nr = TRUE))) > 0L) {
    stop("Cannot create OptPath, measures ids and dimension names of input space overlap!")
  }
  minimize = vlapply(measures, function(m) m$minimize)
  names(minimize) = ns
  makeOptPathDF(par.set, ns, minimize, add.transformed.x = FALSE,
    include.error.message = TRUE, include.exec.time = TRUE, ...)
}


##### featsel #####
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

binaryToFeatures = function(x, all.vars) {
  assertIntegerish(x, lower = 0, upper = 1, len = length(all.vars)) # We want to avoid vector recycling here
  all.vars[as.logical(x)]
}

compare.diff = function(state1, state2, control, measure, threshold) {
  ifelse(measure$minimize, 1, -1) * (state1$y[1] - state2$y[1]) > threshold
}
