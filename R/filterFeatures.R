#' @title Filter features by thresholding filter values.
#'
#' @description
#' First, calls \code{\link{getFilterValues}}.
#' Features are then selected via \code{select} and \code{val}.
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   See \code{\link{listFilterMethods}}.
#'   Default is \dQuote{rf.importance}.
#' @param select [\code{character(1)}]\cr
#'   How to select top-scoring features.
#'   \dQuote{perc} = select top-scoring percentage, \dQuote{abs} = select absolute number
#'   of top-scoring features, \dQuote{threshold} = select all features whose
#'   criterion value is >= \code{val}.
#'   Default is \dQuote{perc}.
#' @param val [\code{numeric(1)}]\cr
#'   Depends on \code{select}:
#'   Either a percentage from [0, 1], a number of features or a threshold value for the criterion.
#' @param ... [any]\cr
#'   Passed down to selected filter method.
#' @template ret_task
#' @export
#' @family filter
filterFeatures = function(task, method = "rf.importance", select = "perc", val, ...) {
  filters = getFilterRegister()
  assertChoice(method, choices = ls(filters))
  checkFilterArguments(select = select, val = val)

  p = getTaskNFeats(task)
  nselect = switch(select,
    perc = round(val * p),
    abs = min(val, p),
    threshold = p
  )
  fval = getFilterValues(task = task, method = method, nselect = nselect, ...)$data
  if (select == "threshold")
    nselect = sum(fval$val >= val, na.rm = TRUE)
  features = as.character(head(sortByCol(fval, "val", asc = FALSE)$name, nselect))
  subsetTask(task, features = features)
}

checkFilterArguments = function(select, val) {
  switch(select,
    perc = assertNumber(val, lower = 0, upper = 1),
    abs = assertCount(val),
    threshold = assertNumber(val)
  )
}
