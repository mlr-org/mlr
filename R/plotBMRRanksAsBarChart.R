#' @title Create a bar chart for ranks in a BenchmarkResult.
#'
#' @description
#' Plots a bar chart from the ranks of algorithms. Alternatively,
#' tiles can be plotted for every rank-task combination, see `pos`
#' for details. In all plot variants the ranks of the learning algorithms are displayed on
#' the x-axis. Areas are always colored according to the `learner.id`.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param ties.method (`character(1)`)\cr
#'   See [rank] for details.
#' @template arg_aggregation_method
#' @param pos (`character(1)`)\cr
#'   Optionally set how the bars are positioned in ggplot2.
#'   Ranks are plotted on the x-axis.
#'   \dQuote{tile} plots a heat map with `task` as the y-axis.
#'   Allows identification of the performance in a special task.
#'   \dQuote{stack} plots a stacked bar plot.
#'   Allows for comparison of learners within and and across ranks.
#'   \dQuote{dodge} plots a bar plot with bars next to each other instead
#'   of stacked bars.
#' @template arg_order_lrns
#' @template arg_order_tsks
#' @template arg_prettynames
#' @template ret_gg2
#' @family plot
#' @family benchmark
#' @export
#' @examples
#' # see benchmark
plotBMRRanksAsBarChart = function(bmr, measure = NULL, ties.method = "average", aggregation = "default",
  pos = "stack", order.lrns = NULL, order.tsks = NULL, pretty.names = TRUE) {

  assertClass(bmr, "BenchmarkResult")
  measure = checkBMRMeasure(measure, bmr)
  assertChoice(pos, c("tile", "stack", "dodge"))
  df = as.data.frame(convertBMRToRankMatrix(bmr, measure, ties.method = ties.method, aggregation = aggregation))
  df$learner.id = factor(rownames(df))

  setDT(df)
  df = melt(df, id.vars = "learner.id")
  setnames(df, c("variable", "value"), c("task.id", "rank"))
  df = orderBMRLrns(bmr, df, order.lrns)
  df = orderBMRTasks(bmr, df, order.tsks)
  if (pretty.names) {
    learner.ids = getBMRLearnerIds(bmr)
    learner.short.names = getBMRLearnerShortNames(bmr)
    checkDuplicatedLearnerNames(learner.short.names)
    names(learner.short.names) = learner.ids
    if (is.null(order.lrns)) {
      learner.short.names = learner.short.names[sort(learner.ids)]
    } else {
      learner.short.names = learner.short.names[order.lrns]
    }
    levels(df$learner.id) = learner.short.names
  }
  df$rank = as.factor(df$rank)
  setDF(df)

  if (pos == "tile") {
    p = ggplot(df, aes_string("rank", "task.id", fill = "learner.id"))
    p = p + geom_raster()
    p = p + ylab(NULL)
  } else {
    p = ggplot(df, aes_string("rank", fill = "learner.id"))
    p = p + geom_bar(position = pos)
    p = p + ylab(NULL)
  }

  return(p)
}
