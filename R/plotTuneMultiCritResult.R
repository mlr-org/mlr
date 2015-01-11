#' @title Plots multi-criteria results after tuning.
#'
#' @description
#' Visualizes the pareto front and possibly the dominated points.
#'
#' @param res [\code{\link{TuneMultiCritResult}}]\cr
#'   Result of \code{\link{tuneParamsMultiCrit}}.
#' @param path [\code{logical(1)}]\cr
#'   Visualize all evaluated points (or only the non-dominated pareto front)?
#'   For the full path, the size of the points on the front is slightly increased.
#'   Default is \code{TRUE}.
#' @param col [\code{character(1)}]\cr
#'   Which column of \code{res$opt.path} should be mapped to ggplot2 color?
#'   Default is \code{NULL}, which means none.
#' @param shape [\code{character(1)}]\cr
#'   Which column of \code{res$opt.path} should be mapped to ggplot2 shape?
#'   Default is \code{NULL}, which means none.
#' @param pointsize [\code{numeric(1)}]\cr
#'   Point size for ggplot2 \code{\link[ggplot2]{geom_point}} for data points.
#'   Default is 2.
#' @template ret_gg2
#' @family tune_multicrit
#' @export
#' @examples
#' # see tuneParamsMultiCrit
plotTuneMultiCritResult = function(res, path = TRUE, col = NULL, shape = NULL, pointsize = 2) {
  assertClass(res, "TuneMultiCritResult")
  assertFlag(path)
  op1 = res$opt.path
  op2 = as.data.frame(op1)
  if (!is.null(col))
    assertChoice(col, choices = colnames(op2))
  if (!is.null(shape))
    assertChoice(shape, colnames(op2))

  names.y = colnames(res$y)
  map = aes_string(x = names.y[1L], y = names.y[2L], col = col, shape = shape)
  i.front = res$ind
  if (path) {
    i.data = seq_row(op2)
  } else {
    i.data = i.front
  }
  data = op2[i.data,, drop = FALSE]
  front = op2[i.front,, drop = FALSE]

  p = ggplot(data, mapping = map)
  p = p + geom_point(size = pointsize)
  if (path)
    p = p + geom_point(data = front, size = pointsize * 1.5)
  return(p)
}

