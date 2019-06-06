#' @title Plots multi-criteria results after tuning using ggplot2.
#'
#' @description
#' Visualizes the pareto front and possibly the dominated points.
#'
#' @param res ([TuneMultiCritResult])\cr
#'   Result of [tuneParamsMultiCrit].
#' @param path (`logical(1)`)\cr
#'   Visualize all evaluated points (or only the non-dominated pareto front)?
#'   For the full path, the size of the points on the front is slightly increased.
#'   Default is `TRUE`.
#' @param col (`character(1)`)\cr
#'   Which column of `res$opt.path` should be mapped to ggplot2 color?
#'   Default is `NULL`, which means none.
#' @param shape (`character(1)`)\cr
#'   Which column of `res$opt.path` should be mapped to ggplot2 shape?
#'   Default is `NULL`, which means none.
#' @param pointsize (`numeric(1)`)\cr
#'   Point size for ggplot2 [ggplot2::geom_point] for data points.
#'   Default is 2.
#' @param pretty.names (`logical(1)`)\cr
#'   Whether to use the ID of the measures instead of their name in labels. Defaults to `TRUE`.
#' @template ret_gg2
#' @family tune_multicrit
#' @export
#' @examples
#' # see tuneParamsMultiCrit
plotTuneMultiCritResult = function(res, path = TRUE, col = NULL, shape = NULL, pointsize = 2, pretty.names = TRUE) {

  assertClass(res, "TuneMultiCritResult")
  assertFlag(path)
  op1 = res$opt.path
  op2 = as.data.frame(op1)
  if (!is.null(col)) {
    assertChoice(col, choices = colnames(op2))
  }
  if (!is.null(shape)) {
    assertChoice(shape, colnames(op2))
  }

  names.y = colnames(res$y)[1:2]

  map = aes_string(x = names.y[1L], y = names.y[2L], col = col, shape = shape)
  i.front = res$ind
  if (path) {
    i.data = seq_row(op2)
  } else {
    i.data = i.front
  }
  data = op2[i.data, , drop = FALSE]
  front = op2[i.front, , drop = FALSE]

  p = ggplot(data, mapping = map)
  p = p + geom_point(size = pointsize)
  if (path) {
    p = p + geom_point(data = front, size = pointsize * 1.5)
  }
  if (pretty.names) {
    names.y = sapply(res$measures, function(x) x$id)
    p = p + labs(x = names.y[1L], y = names.y[2L])
  }
  return(p)
}
