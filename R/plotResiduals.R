#' @title Create residual plots for prediction objects or benchmark results.
#'
#' @description
#' FIXME
#'
#' @param obj [\code{\link{Prediction}} | \code{\link{BenchmarkResult}}]\cr
#'   Input data.
#' @param type Type of plot. Can be \dQuote{scatterplot}, the default. Or
#'   \dQuote{hist}, for a histogram of the residuals.
#' @param loess.smooth [\code{logical(1)}]\cr
#'   Should a loess smoother be added to the plot? Defaults to \code{TRUE}.
#'   Only applicable if \code{type} is set to \code{scatterplot}.
#' @param rug [\code{logical(1)}]\cr
#'   Should marginal distributions be added to the plot? Defaults to \code{TRUE}.
#'   Only applicable if \code{type} is set to \code{scatterplot}.
#' @param pretty.names [\code{logical(1)}]\cr
#'   If \code{TRUE}, the default, learner short names will be printed.
#' @template ret_gg2
#' @family plot
#' @export
#' @examples
#' # see benchmark
plotResiduals = function(obj, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE, pretty.names = TRUE) {
  
  assertChoice(type, c("scatterplot", "hist"))
  assertLogical(loess.smooth, len = 1L)
  assertLogical(rug, len = 1L)
  assertLogical(pretty.names, len = 1L)
  UseMethod("plotResiduals")
}

#' @export
plotResiduals.Prediction = function(obj, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE, pretty.names = TRUE) {

  task.type = obj$task.desc$type
  if (task.type %nin% c("regr", "classif"))
    stopf("Task type must be 'regr' or 'classif'. But has type '%s'.", task.type)

  df = as.data.frame(obj)

  p = makeResidualPlot(df, type, loess.smooth, rug)

  return(p)
}

#' @export
plotResiduals.BenchmarkResult = function(obj, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE, pretty.names = TRUE) {

  # FIXME: We want a getter for the task.desc. PR 914 will implement this.
  bmr.preds = unlist(getBMRPredictions(bmr), recursive = FALSE)
  task.type = unique(extractSubList(bmr.preds, c("task.desc", "type")))
  if (task.type %nin% c("regr", "classif"))
    stopf("Task type must be 'regr' or 'classif'. But has type '%s'.", task.type)

  df = getBMRPredictions(obj, as.df = TRUE)

  p = makeResidualPlot(df, type, loess.smooth, rug)

  p = p + facet_wrap(learner.id ~ task.id)

  return(p)
}


makeResidualPlot = function(df, type = "scatterplot", loess.smooth = TRUE,
  rug = TRUE) {

  if (type == "scatterplot") {
    p = ggplot(df, aes_string("truth", "response")) + geom_count()

    if (loess.smooth)
      p = p + geom_smooth(se = FALSE)
    if (rug)
      p = p + geom_rug(color = "red")

    p = p + ggtitle("True value vs. fitted value")

  } else {
    df$residuals = as.numeric(df$truth) - as.numeric(df$response)
    p = ggplot(df, aes_string("residuals")) + geom_histogram()
    p = p + ggtitle("Histogram of residuals")
  }

  return(p)
}

