#' @title Generate data to plot a rank-matrix as a barplot.
#'
#' @description Generate Data for \code{\link{plotRankMatrixAsBar}}.
#' Ties in ranks are broken randomly, in order to assert non-overlapping
#' and non-empty bars.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param pos [\code{character(1)}]
#'   Optionally set how the bars are positioned in \code{\link{ggplot2}}. \cr
#'   Ranks are plotted on the x-axis. \cr
#'   \dQuote{tile} plots a heatmap with \code{tasks} as the y-axis.\cr
#'   Allows identification of the performance in a special task.\cr
#'   \dQuote{stack} plots a stacked barplot. \cr
#'   Allows for comparison of learners within and and accross ranks.\cr
#'   \dQuote{dodge} plots a barplot with bars next to each other instead
#'   of stacked bars.\cr
#' @template arg_order_lrns
#' @template arg_order_tsks
#' @return [\code{RankMatrixAsBarData}]. List with items:
#' \item{data}{\code{data.frame} containing the data for plotting}
#' \item{measure}{measure the ranks are calculated on}
#' \item{pos}{Positioning info for the plot}
#'
#' @examples
#' # see plotRankMatrixAsBar
#' @family generate_plot_data
#' @family benchmark
#' @export
generateRankMatrixAsBarData = function(bmr, measure = NULL, pos = "tile",
                                       order.lrns = NULL, order.tsks = NULL) {
  # Assert Classes and fill NULL
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertChoice(pos, c("tile", "stack", "dodge"))

  # melt back into plotable form:
  df = convertBMRToRankMatrix(bmr, measure, ties.method = "random")
  df = melt(df)
  colnames(df) = c("learner.id", "task.id", "Rank")

  # Order if needed
  if (!is.null(order.lrns))
    df = orderBMRLrns(bmr, df, order.lrns)
  if (!is.null(order.tsks))
    df = orderBMRTasks(bmr, df, order.tsks)

  makeS3Obj("RankMatrixAsBarData",
            "data" = df,
            "pos" = pos,
            "measure" = measure)
}
#' @title Plot a rank-matrix as a barplot.
#'
#' @description Plots a barchart from the ranks of algorithms. Alternatively
#' tiles can be plotted for every rank-task combination, see \code{pos}
#' for details. The x-axis accross all plots is the ranks of a learner.id.
#' Areas are always coloured corresponding to the learner.
#'
#' @param obj [\code{RankMatrixAsBarData}]\cr
#'   Output of a \code{\link{generateRankMatrixAsBarData}} function.
#' @param pos [\code{character(1)}]
#'   Optionally set how the bars are positioned in \code{\link{ggplot2}}. \cr
#'   Overwrites the one created in \code{\link{generateRankMatrixAsBarData}}. \cr
#'   \dQuote{tile} plots a heatmap with \code{task} as the y-axis.\cr
#'   Allows identification of the performance in a special task.\cr
#'   \dQuote{stack} plots a stacked barplot. \cr
#'   Allows for comparison of learners within and and accross ranks.\cr
#'   \dQuote{dodge} plots a barplot with bars next to each other instead
#'   of stacked bars.\cr
#' @template ret_gg2
#'
#' @examples
#' lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
#' tasks = list(iris.task, sonar.task)
#' rdesc = makeResampleDesc("CV", iters = 2L)
#' meas = list(acc, mmce)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' r = generateRankMatrixAsBarData(res)
#' plotRankMatrixAsBar(r, "tile")
#'
#' @family plot
#' @family benchmark
#' @export
plotRankMatrixAsBar = function(obj, pos = NULL) {
  assertClass(obj, "RankMatrixAsBarData")
  if (is.null(pos))
    pos = obj$pos
  assertChoice(pos, c("tile", "stack", "dodge"))

  obj$data$Rank = as.factor(obj$data$Rank)
  if (pos == "tile") {
    p = ggplot(obj$data, aes_string("Rank", "task.id", fill = "learner.id"))
    p = p + geom_tile()
  } else {
    p = ggplot(obj$data, aes_string("Rank", fill = "learner.id"))
    p = p + geom_bar(position = pos)
    p = p + ylab(NULL)
  }
  return(p)
}
