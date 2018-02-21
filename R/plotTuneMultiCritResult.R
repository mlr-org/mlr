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
  if (!is.null(col))
    assertChoice(col, choices = colnames(op2))
  if (!is.null(shape))
    assertChoice(shape, colnames(op2))

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
  if (path)
    p = p + geom_point(data = front, size = pointsize * 1.5)
  if (pretty.names) {
    names.y = sapply(res$measures, function(x) x$id)
    p = p + labs(x = names.y[1L], y = names.y[2L])
  }
  return(p)
}
#' @title Plots multi-criteria results after tuning using ggvis.
#'
#' @description
#' Visualizes the pareto front and possibly the dominated points.
#'
#' @param res ([TuneMultiCritResult])\cr
#'   Result of [tuneParamsMultiCrit].
#' @param path (`logical(1)`)\cr
#'   Visualize all evaluated points (or only the non-dominated pareto front)?
#'   Points are colored according to their location.
#'   Default is `TRUE`.
#' @param point.info (`character(1)`)\cr
#'   Show for each point which hyper parameters led to this point?
#'   For `"click"`, information is displayed on mouse click.
#'   For `"hover"`, information is displayed on mouse hover.
#'   If set to `"none"`, no information is displayed.
#'   Default is `"hover"`.
#' @param point.trafo (`logical(1)`)\cr
#'   Should the information show the transformed hyper parameters?
#'   Default is `TRUE`.
#'
#' @template ret_ggv
#' @family tune_multicrit
#' @export
#' @examples
#' # see tuneParamsMultiCrit
plotTuneMultiCritResultGGVIS = function(res, path = TRUE, point.info = "hover", point.trafo = TRUE) {
  requirePackages("_ggvis")
  assertClass(res, "TuneMultiCritResult")
  assertFlag(path)
  assertChoice(point.info, choices = c("click", "hover", "none"))
  assertFlag(point.trafo)

  plt.data = as.data.frame(res$opt.path)
  plt.data$location = factor(row.names(plt.data) %in% res$ind, levels = c(TRUE, FALSE),
                             labels = c("frontier", "interior"))
  plt.data$id = seq_len(nrow(plt.data))

  if (point.trafo) {
    for (param in res$opt.path$par.set$pars) {
      plt.data[, param$id] = trafoValue(param, plt.data[, param$id])
    }
  }

  if (!path) {
    plt.data = plt.data[plt.data$location == "frontier", , drop = FALSE]
  }

  info = function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    n = length(res$x[[1]])
    row = plt.data[plt.data$id == x$id, ][1:n]
    text = paste0(names(row), ": ", format(row, zero.print = TRUE), collapse = "<br />")
    return(text)
  }

  plt = ggvis::ggvis(plt.data, ggvis::prop("x", as.name(colnames(res$y)[1L])),
    ggvis::prop("y", as.name(colnames(res$y)[2L])),
    ggvis::prop("key", ~id, scale = FALSE))
  plt = ggvis::layer_points(plt, ggvis::prop("fill", as.name("location")))

  if (point.info != "none") {
    plt = ggvis::add_tooltip(plt, info, point.info)
  }

  return(plt)
}
