#' @title Filter features by thresholding filter values.
#'
#' @description
#' First, calls \code{\link{getFilterValues}}.
#' Features are then selected via \code{select} and \code{val}.
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   See \code{\link{getFilterValues}}.
#'   Default is \dQuote{random.forest.importance}.
#' @param select [\code{character(1)}]\cr
#'   How to select top-scoring features.
#'   \dQuote{perc} = select top-scoring percentage, \dQuote{abs} = select absolute number
#'   of top-scoring features, \dQuote{threshold} = select all features whose
#'   criterion value is >= \code{val}.
#'   Default is \dQuote{perc}.
#' @param val  [\code{numeric(1)}]\cr
#'   Depends on \code{select}:
#'   Either a percentage from [0, 1], a number of features or a threshold value for the criterion.
#' @param ... [any]\cr
#'   Passed down to selected method.
#' @template ret_task
#' @export
#' @family filter
filterFeatures = function(task, method = "random.forest.importance", select = "perc", val, ...) {
  # does checks + loads FSelector
  checkArg(method, choices = listFilterMethods())
  checkFilterArguments(select = select, val = val)
  fvals = getFilterValues(task = task, method = method, ...)
  d = fvals$data
  p = nrow(d)
  nfirst = switch(select,
    perc = round(val * p),
    abs = val,
    sum(d$val >= val)
  )
  nfirst = min(max(0, nfirst), p)
  d = sortByCol(d, "val", asc = FALSE)
  subsetTask(task, features = d$name[seq_len(nfirst)])
}

checkFilterArguments = function(select, val) {
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

