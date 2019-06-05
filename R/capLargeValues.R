#' @title Convert large/infinite numeric values in a data.frame or task.
#'
#' @description
#' Convert numeric entries which large/infinite (absolute) values
#' in a data.frame or task.
#' Only numeric/integer columns are affected.
#'
#' @template arg_taskdf
#' @param target ([character])\cr
#'   Name of the column(s) specifying the response.
#'   Target columns will not be capped.
#'   Default is `character(0)`.
#' @param cols ([character])\cr
#'   Which columns to convert.
#'   Default is all numeric columns.
#' @param threshold (`numeric(1)`)\cr
#'   Threshold for capping.
#'   Every entry whose absolute value is equal or larger is converted.
#'   Default is `Inf`.
#' @param impute (`numeric(1)`)\cr
#'   Replacement value for large entries.
#'   Large negative entries are converted to `-impute`.
#'   Default is `threshold`.
#' @param what (`character(1)`)\cr
#'   What kind of entries are affected?
#'   \dQuote{abs} means `abs(x) > threshold`,
#'   \dQuote{pos} means `abs(x) > threshold && x > 0`,
#'   \dQuote{neg} means `abs(x) > threshold && x < 0`.
#'   Default is \dQuote{abs}.
#' @return ([data.frame])
#' @export
#' @family eda_and_preprocess
#' @examples
#' capLargeValues(iris, threshold = 5, impute = 5)
capLargeValues = function(obj, target = character(0L), cols = NULL,
  threshold = Inf, impute = threshold, what = "abs") {
  checkTargetPreproc(obj, target, cols)
  assertNumber(threshold, lower = 0)
  assertNumber(impute, lower = 0)
  assertChoice(what, c("abs", "pos", "neg"))
  UseMethod("capLargeValues")
}

#' @export
capLargeValues.Task = function(obj, target = character(0L), cols = NULL,
  threshold = Inf, impute = threshold, what = "abs") {
  d = getTaskData(obj)
  d = capLargeValues.data.frame(d, target = character(0L), cols = cols,
    threshold = threshold, impute = impute)
  changeData(obj, data = d)
}

#' @export
capLargeValues.data.frame = function(obj, target = character(0L), cols = NULL,
  threshold = Inf, impute = threshold, what = "abs") {

  allnumfeats = colnames(obj)[vlapply(obj, is.numeric)]
  allnumfeats = setdiff(allnumfeats, target)

  # check that user requested cols are only numeric cols with the target
  if (!is.null(cols)) {
    assertSubset(cols, allnumfeats)
  } else {
    cols = allnumfeats
  }

  fun = switch(what,
    abs = function(x) abs(x) > threshold,
    pos = function(x) abs(x) > threshold & x > 0,
    neg = function(x) abs(x) > threshold & x < 0
  )

  for (cn in cols) {
    x = obj[[cn]]
    ind = which(fun(x))
    if (length(ind) > 0L) {
      obj[ind, cn] = ifelse(x[ind] > threshold, impute, -impute)
    }
  }
  return(obj)
}
