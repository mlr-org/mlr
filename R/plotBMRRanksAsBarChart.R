#' @title Create a bar-chart for ranks in a BenchmarkResult.
#'
#' @description
#' Plots a barchart from the ranks of algorithms. Alternatively
#' tiles can be plotted for every rank-task combination, see \code{pos}
#' for details. The x-axis accross all plots is the ranks of a learner.id.
#' Areas are always coloured corresponding to the learner.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param ties.method [\code{character(1)}]\cr
#'   See \code{\link{rank}} for details.
#' @template arg_aggregation_method
#' @param pos [\code{character(1)}]
#'   Optionally set how the bars are positioned in ggplot2.
#'   Ranks are plotted on the x-axis.
#'   \dQuote{tile} plots a heatmap with \code{task} as the y-axis.
#'   Allows identification of the performance in a special task.
#'   \dQuote{stack} plots a stacked barplot. r
#'   Allows for comparison of learners within and and accross ranks.
#'   \dQuote{dodge} plots a barplot with bars next to each other instead
#'   of stacked bars.
#' @template arg_order_lrns
#' @template arg_order_tsks
#' @template ret_gg2
#' @family plot
#' @family benchmark
#' @export
#' @examples
#' # see benchmark
plotBMRRanksAsBarChart = function(bmr, measure = NULL, ties.method = "average", aggregation = "default", pos = "stack", order.lrns = NULL, order.tsks = NULL) {
  assertClass(bmr, "BenchmarkResult")
  measure = checkBMRMeasure(measure, bmr)
  assertChoice(pos, c("tile", "stack", "dodge"))

  df = convertBMRToRankMatrix(bmr, measure, ties.method = ties.method, aggregation = aggregation)

  # melt back into plotable form:
  df = melt(df)
  colnames(df) = c("learner.id", "task.id", "rank")
  df = orderBMRLrns(bmr, df, order.lrns)
  df = orderBMRTasks(bmr, df, order.tsks)

  df$rank = as.factor(df$rank)
  if (pos == "tile") {
    p = ggplot(df, aes_string("rank", "task.id", fill = "learner.id"))
    p = p + geom_tile()
    p = p + ylab(NULL)
  } else {
    p = ggplot(df, aes_string("rank", fill = "learner.id"))
    p = p + geom_bar(position = pos)
    p = p + ylab(NULL)
  }
  return(p)
}
