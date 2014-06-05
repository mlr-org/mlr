#' @title Filter features by thresholding importance values.
#'
#' @description
#' First, calls \code{\link{getFilterValues}}.
#' Features are then selected via one of: \code{fw.perc}, \code{fw.n} or \code{fw.threshold}.
#'
#' @template arg_taskdf
#' @template arg_target12
#' @param method [\code{character(1)}]\cr
#'   See \code{\link{getFilterValues}}.
#'   Default is \dQuote{random.forest.importance}.
#' @param select [\code{character(1)}]\cr
#'   How to select top features.
#'   \dQuote{perc} = select top-scoring percentage, \dQuote{abd} = select number
#'   of top-scoring features, \dQuote{threshold} = select all features whose
#'   criterion value is >= \code{val}.
#'   Default is \dQuote{perc}.
#' @param val  [\code{numeric(1)}]\cr
#'   Depends on \code{select}:
#'   Either a percentage from [0, 1], a number of features or a threshold value for the criterion.
#' @return [\code{data.frame} | \code{\link{SupervisedTask}}]. Same type as \code{obj}.
#' @export
#' @family filter
filterFeatures = function(obj, target, method = "random.forest.importance", select = "perc", val) {
  checkArg(obj, c("data.frame", "SupervisedTask"))
  val = checkFilterArguments(method, select, val)
  UseMethod("filterFeatures")
}

#' @export
filterFeatures.SupervisedTask = function(obj, target, method = "random.forest.importance", select = "perc", val) {
  d = filterFeatures(getTaskData(obj), obj$task.desc$target, method, select, val)
  changeData(obj, d)
}

#' @export
filterFeatures.data.frame = function(obj, target, method = "random.forest.importance", select = "perc", val) {

  requirePackages("FSelector")
  checkArg(target, "character")
  fvals = getFilterValues(obj, target, method)
  p = length(fvals)
  nfirst = switch(select,
    perc = round(val * p),
    abs = val,
    sum(fvals >= val)
  )
  fvals = sort(fvals, decreasing = TRUE)
  feats = if (nfirst < 1)
    character(0)
  else if (nfirst >= p)
    setdiff(colnames(obj), target)
  else
    feats = names(fvals)[1:nfirst]
  obj[, colnames(obj) %in% c(feats, target), drop = FALSE] #preserves order!
}

checkFilterArguments = function(method, select, val) {
  checkArg(method, choices = getFilterMethods())
  checkArg(select, choices = c("perc", "abs", "threshold"))
  switch(select,
    perc = checkArg(val, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1),
    abs = {
      val = convertInteger(val)
      checkArg(val, "integer", len = 1L, na.ok = FALSE, lower = 0)
    },
    threshold = checkArg(val, "numeric", len = 1L, na.ok = FALSE)
  )
}

