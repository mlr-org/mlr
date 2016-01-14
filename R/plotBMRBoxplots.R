#' @title Create box or violin plots for a BenchmarkResult.
#'
#' @description
#' Plots box or violin plots for a selected \code{measure} across all iterations
#' of the resampling strategy, faceted by the \code{task.id}.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param style [\code{character(1)}]\cr
#'   Type of plot, can be \dQuote{box} for a boxplot or \dQuote{violin} for a violin plot.
#'   Default is \dQuote{box}.
#' @param pretty.names [\code{logical(1)}]\cr
#'   Whether to use the \code{\link{Measure}} name instead of the id in the plot.
#'   Default is \code{TRUE}.
#' @template arg_order_lrns
#' @template arg_order_tsks
#' @template ret_gg2
#' @family plot
#' @family benchmark
#' @export
#' @examples
#' # see benchmark
plotBMRBoxplots = function(bmr, measure = NULL, style = "box", order.lrns = NULL, order.tsks = NULL, pretty.names = TRUE) {

  assertClass(bmr, "BenchmarkResult")
  measure = checkBMRMeasure(measure, bmr)
  assertChoice(style, c("box", "violin"))

  df = as.data.frame(bmr)
  df = orderBMRLrns(bmr, df, order.lrns)
  df = orderBMRTasks(bmr, df, order.tsks)

  p = ggplot(df, aes_string("learner.id", measure$id))
  p = p + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -45, hjust = 0))
  p = p + facet_grid(. ~ task.id)

  if (pretty.names)
    p = p + ylab(measure$name)

  if (style == "box")
    p = p + geom_boxplot()
  else
    p = p + geom_violin() + stat_summary(fun.ymin = median, fun.ymax = median, fun.y = median, geom = "crossbar")
  return(p)
}
