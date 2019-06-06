#' @title Summarize columns of data.frame or task.
#'
#' @description
#' Summarizes a data.frame, somewhat differently than the normal [summary] function of R.
#' The function is mainly useful as a basic EDA tool on data.frames before they are converted to tasks,
#' but can be used on tasks as well.
#'
#' Columns can be of type numeric, integer, logical, factor, or character.
#' Characters and logicals will be treated as factors.
#'
#' @template arg_taskdf
#' @return ([data.frame]). With columns:
#'   \item{name}{Name of column.}
#'   \item{type}{Data type of column.}
#'   \item{na}{Number of NAs in column.}
#'   \item{disp}{Measure of dispersion, for numerics and integers [sd] is used, for
#'     categorical columns the qualitative variation.}
#'   \item{mean}{Mean value of column, NA for categorical columns.}
#'   \item{median}{Median value of column, NA for categorical columns.}
#'   \item{mad}{MAD of column, NA for categorical columns.}
#'   \item{min}{Minimal value of column, for categorical columns the size of the smallest category.}
#'   \item{max}{Maximal value of column, for categorical columns the size of the largest category.}
#'   \item{nlevs}{For categorical columns, the number of factor levels, NA else.}
#' @export
#' @family eda_and_preprocess
#' @examples
#' summarizeColumns(iris)
summarizeColumns = function(obj) {
  UseMethod("summarizeColumns")
}

#' @export
summarizeColumns.Task = function(obj) {
  summarizeColumns.data.frame(obj$env$data)
}

#' @export
summarizeColumns.data.frame = function(obj) {

  iqv = function(x, ...) {
    1 - mean(x == computeMode(x))
  }

  # to be read as: is obj is numeric, return x, else call y(x)
  ifn = function(obj, x, y, ...) {
    if (is.numeric(obj)) y = x
    if (is.function(y)) y(obj, ...) else y
  }

  res = namedList(c("name", "type", "na", "mean", "disp", "median", "mad", "min", "max", "nlevs"))
  res$name = colnames(obj)
  res$type = vcapply(obj, function(x) class(x)[1L], use.names = FALSE)
  res$na = viapply(obj, function(x) sum(is.na(x)), use.names = FALSE)
  res$mean = vnapply(obj, ifn, mean, NA, use.names = FALSE, na.rm = TRUE)
  res$disp = vnapply(obj, ifn, sd, iqv, use.names = FALSE, na.rm = TRUE)
  res$median = vnapply(obj, ifn, median, NA, use.names = FALSE, na.rm = TRUE)
  res$mad = vnapply(obj, ifn, mad, NA, use.names = FALSE, na.rm = TRUE)
  res$min = vnapply(obj, ifn, min, function(x, ...) min(table(x), ...), use.names = FALSE, na.rm = TRUE)
  res$max = vnapply(obj, ifn, max, function(x, ...) max(table(x), ...), use.names = FALSE, na.rm = TRUE)
  res$nlevs = viapply(obj, ifn, 0L, function(x) length(levels(factor(x))), use.names = FALSE)

  as.data.frame(res, stringsAsFactors = FALSE)
}
