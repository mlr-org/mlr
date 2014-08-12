#' @title Summarize columns of data.frame or task.
#'
#' @description
#' Summarizes a data.frame, somewhat differently than the normal \code{\link{summary}} function of R.
#' The function is mainly useful as a basic EDA tool on data.frames before they are converted to tasks,
#' but can be used on tasks as well.
#'
#' Columns can be of type numeric, integer, logical, factor, or character.
#' Characters and logicals will be treated as factors.
#'
#' @template arg_taskdf
#' @return [\code{data.frame}]. With columns:
#'   \item{name}{Name of column.}
#'   \item{type}{Data type of column.}
#'   \item{na}{Number of NAs in column.}
#'   \item{disp}{Measure of dispersion, for numerics and integers \code{\link{sd}} is used, for
#'     categorical columns the qualitative variation.}
#'   \item{mean}{Mean value of column, NA for categorical columns}
#'   \item{median}{Median value of column, NA for categorical columns.}
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
  iqv = function(x) {
    1 - mean(x == computeMode(x))
  }
  n = ncol(obj)
  cns = colnames(obj)
  res = data.frame(
    name = character(n),
    type = character(n),
    na = integer(n),
    disp = numeric(n),
    mean = numeric(n),
    median = numeric(n),
    min = numeric(n),
    max = numeric(n),
    nlevs = integer(n),
    stringsAsFactors = FALSE
  )

  for (i in 1:n) {
    x = obj[,i]
    res[i, "na"] = sum(is.na(x))
    x = na.omit(x)
    res[i, "name"] = cns[i]
    res[i, "type"] = class(x)[1]
    if (is.numeric(x) | is.integer(x)) {
      res[i, "disp"] = sd(x)
      res[i, "mean"] = mean(x)
      res[i, "median"] = median(x)
      res[i, "min"] = min(x)
      res[i, "max"] = max(x)
      res[i, "nlevs"] = as.integer(NA)
    } else if (is.factor(x) | is.logical(x) | is.character(x)) {
      x = as.factor(x)
      tab = table(x)
      res[i, "disp"] = iqv(x)
      res[i, "mean"] = NA
      res[i, "median"] = NA
      res[i, "min"] = min(tab)
      res[i, "max"] = max(tab)
      res[i, "nlevs"] = length(levels(x))
    } else {
      stop("Unsupported column class: ", class(x))
    }
  }
  return(res)
}
