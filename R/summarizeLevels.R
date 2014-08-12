#' @title Summarizes factors of a data.frame by tabling them.
#'
#' @description
#' Characters and logicals will be treated as factors.
#'
#' @template arg_taskdf
#' @param cols [\code{character}]\cr
#'   Restrict result to columns in \code{cols}.
#'   Default is all factor, character and logical columns of \code{obj}.
#' @return [\code{list}]. Named list of tables.
#' @export
#' @family eda_and_preprocess
#' summarizeLevels(iris)
summarizeLevels = function(obj, cols = NULL) {
  UseMethod("summarizeLevels")
}

#' @export
summarizeLevels.Task = function(obj, cols) {
  summarizeLevels.data.frame(obj$env$data, cols = NULL)
}

#' @export
summarizeLevels.data.frame = function(obj, cols = NULL) {
  n = ncol(obj)
  cns = colnames(obj)
  pred = function(x) is.factor(x) || is.logical(x) || is.character(x)
  okcols = Filter(function(x) pred(obj[,x]), cns)
  if (is.null(cols))
    cols = okcols
  else
    assertSubset(cols, okcols)
  res = list()
  for (x in cols) {
     res[[x]] = table(as.factor(obj[,x]))
  }
  return(res)
}
