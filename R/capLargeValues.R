#' @title Convert large/infinite numeric values in a data.frame or task.
#'
#' @description
#' Convert numeric entries which large/infinite (absolute) values in a data.frame
#' Only numeric/integer columns are affected.
#'
#' @template arg_taskdf
#' @param cols [\code{character}]
#'   Which columns to convert.
#'   Default is all numeric columns.
#' @param threshold [\code{numeric(1)}]\cr
#'   Threshold for capping.
#'   Every entry whose absolute value is equal or larger is converted.
#'   Default is \code{Inf}.
#' @param impute [\code{numeric(1)}]\cr
#'   Replacement value for large entries.
#'   Large negative entries are converted to \code{-impute}.
#'   Default is \code{threshold}.
#' @param what [character(1)]
#'   What kind of entries are affected?
#'   \dQuote{abs} means \code{abs(x) > threshold},
#'   \dQuote{pos} means \code{abs(x) > threshold && x > 0},
#'   \dQuote{pos} means \code{abs(x) > threshold && x < 0},
#'   Default is \dQuote{abs}
#' @return [\code{data.frame}]
#' @export
#' @family eda_and_preprocess
#' @examples
#' capLargeValues(iris, threshold = 5, impute = 5)
capLargeValues = function(obj, cols = NULL, threshold = Inf, impute = threshold, what = "abs") {
  assertNumber(threshold, lower = 0)
  assertNumber(impute, lower = 0)
  assertChoice(what, c("abs", "pos", "neg"))
  UseMethod("capLargeValues")
}

#' @export
capLargeValues.Task = function(obj, cols = NULL, threshold = Inf, impute = threshold, what = "abs") {
  d = capLargeValues.data.frame(obj$env$obj, cols = cols, threshold = threshold, impute = impute)
  changeData(obj, data = d)
}

#' @export
capLargeValues.data.frame = function(obj, cols = NULL, threshold = Inf, impute = threshold, what = "abs") {
  cns = colnames(obj)
  # select numeric and int cols
  numcols = sapply(obj, function(x) is.numeric(x))
  numcols = cns[numcols]
  if (is.null(cols)) {
    cols = numcols
  } else {
    assertSubset(cols, numcols)
  }
  if (length(cols) == 0L)
    return(obj)
  selfun = switch(what,
    abs = function(x) abs(x) > threshold,
    pos = function(x) abs(x) > threshold & x > 0,
    neg = function(x) abs(x) > threshold & x < 0
  )
  for (k in cols) {
    x = obj[, k]
    inds = selfun(x)
    if (any(inds)) {
      inds1 = inds & x > threshold
      inds2 = inds & x < threshold
      obj[inds1, k] = impute
      obj[inds2, k] = -impute
    }
  }
  return(obj)
}
