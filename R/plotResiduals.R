#' @title Create residual plots for prediction objects or benchmark results.
#'
#' @description
#' FIXME
#'
#' @param obj [\code{\link{Prediction}} | \code{\link{BenchmarkResult}}]\cr
#'   Input data.
#' @param type Type of plot. Can be \dQuote{scatterplot}, the default. Or
#' @param loess.smooth [\code{logical(1)}]\cr
#'   Should a loess smoother be added to the plot? Defaults to \code{TRUE}.
#'   Only applicable if \code{type} is set to \code{scatterplot}.
#' @param pretty.names [\code{logical(1)}]\cr
#'   If \code{TRUE}, the default, learner short names will be printed.
#' @template ret_gg2
#' @family plot
#' @export
#' @examples
#' # see benchmark
plotResiduals = function(obj, loess.smooth = TRUE) {
  assertLogical(loess.smooth, len = 1L)
  UseMethod("plotResiduals")
}

#' @export
plotResiduals.Prediction = function(obj, loess.smooth = TRUE) {

  task.type = obj$task.desc$type
  if (task.type %nin% c("regr", "classif"))
    stop("Unsupported type")

  df = as.data.frame(obj)

  p = ggplot(df, aes_string("truth", "response")) + geom_point()
  p = p + ggtitle("True value vs. fitted value")
  
  if (loess.smooth)
    p = p + stat_smooth(se = FALSE)

  return(p)
}

#' @export
plotResiduals.BenchmarkResult = function(obj, loess.smooth = TRUE) {
  # FIXME: We want a getter for the task.desc. PR 914 will implement this.
  bmr.preds = unlist(getBMRPredictions(bmr), recursive = FALSE)
  task.types = extractSubList(bmr.preds, c("task.desc", "type"))
  if (any(task.types %nin% c("regr", "classif")))
    stop("Unsupported type")

  preds = getBMRPredictions(obj, as.df = TRUE)

  p = ggplot(preds, aes_string("truth", "response")) + geom_point()
  p = p + facet_wrap(learner.id ~ task.id)
  p = p + ggtitle("True value vs. fitted value")
  
  if (loess.smooth)
    p = p + stat_smooth(se = FALSE)

  return(p)
}

