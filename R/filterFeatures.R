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
#' @param fval [\code{\link{FilterValues}}]\cr
#'   Result of \code{\link{getFilterValues}}.
#'   If you pass this, the filter values in the object are used for feature filtering.
#'   \code{method} and \code{...} are ignored then.
#'   Default is \code{NULL} and not used.
#' @param perc [\code{numeric(1)}]\cr
#'   If set, select \code{perc}*100 top scoring features.
#'   Mutually exclusive with arguments \code{abs} and \code{threshold}.
#' @param abs [\code{numeric(1)}]\cr
#'   If set, select \code{abs} top scoring features.
#'   Mutually exclusive with arguments \code{perc} and \code{threshold}.
#' @param threshold [\code{numeric(1)}]\cr
#'   If set, select features whose score exceeds \code{threshold}.
#'   Mutually exclusive with arguments \code{perc} and \code{abs}.
#' @param ... [any]\cr
#'   Passed down to selected filter method.
#' @template ret_task
#' @export
#' @family filter
filterFeatures = function(task, method = "rf.importance", fval = NULL, perc = NULL, abs = NULL, threshold = NULL, ...) {
  assertClass(task, "SupervisedTask")
  assertChoice(method, choices = ls(.FilterRegister))
  select = checkFilterArguments(perc, abs, threshold)
  p = getTaskNFeats(task)
  nselect = switch(select,
    perc = round(perc * p),
    abs = min(abs, p),
    threshold = p
  )

  if (is.null(fval)) {
    fval = getFilterValues(task = task, method = method, nselect = nselect, ...)$data
  } else {
    assertClass(fval, "FilterValues")
    fval = fval$data
  }
  if (select == "threshold")
    nselect = sum(fval$val >= threshold, na.rm = TRUE)
  features = as.character(head(sortByCol(fval, "val", asc = FALSE)$name, nselect))
  subsetTask(task, features = features)
}

checkFilterArguments = function(perc, abs, threshold) {
  sum.null = sum(!is.null(perc), !is.null(abs), !is.null(threshold))
  if (sum.null == 0L)
    stop("At least one of 'perc', 'abs' or 'threshold' must be not NULL")
  if (sum.null >= 2L)
    stop("Arguments 'perc', 'abs' and 'threshold' are mutually exclusive")

  if (!is.null(perc)) {
    assertNumber(perc, lower = 0, upper = 1)
    return("perc")
  }
  if (!is.null(abs)) {
    assertCount(abs)
    return("abs")
  }
  if (!is.null(threshold)) {
    assertNumber(threshold)
    return("threshold")
  }
}
