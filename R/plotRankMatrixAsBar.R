#' @title Generate data to plot a rank-matrix as a barplot.
#' 
#' @description Generate Data for \code{\link{plotRankMatrixAsBar}}.
#'  Ties in ranks are broken randomly, in order to assert non-overlapping
#'  and non-empty bars.
#' 
#' @return [\code{RankMatrixAsBarData}] \cr
#' List which contains the following info: \cr
#' $\code{data}: \code{data.frame} containing the data for plotting\cr
#' $\code{measure}: \link{Measure} the ranks are calculated on\cr
#' $\code{pos}: Positioning info for the plot\cr
#'  
#' @param bmr [\code{\link{BenchmarkResult}}] \cr
#'  Output of a \code{\link{benchmark}} function.
#' @param measure [\code{\link{Measure}}] \cr
#'  Measure for which ranks should be calculated (e.g: acc).
#'  Defaults to first.
#' @param pos [\code{character(1)}]
#'  Optionally set how the bars are positioned in [\link{ggplot2}]. \cr
#'  \code{"tile"} plots a heatmap with \link{task} as the y-axis.\cr
#'  Allows identification of the performance in a special task.\cr
#'  \code{"stack"} plots a stacked barplot. \cr
#'  Allows for comparison of learners within and and accross ranks.\cr
#'  \code{"dodge"} plots a barplot of with dodged instead of stacked bars.\cr 
#' @param order.lrns [\code{character(n.learners)}] or \cr 
#'                  [\code{integer(n.learners)}] \cr
#' Character vector with \code{learner.ids} in new order, or integer
#' vector refering to the positions in the new order, that has the number of
#' of learners as length.
#' @param order.tsks [\code{character(n.tasks)}] or \cr 
#'                  [\code{integer(n.tasks)}] \cr
#' Character vector with \code{task.ids} in new order, or an integer
#' vector refering to the positions in the new order, that has the number of learners 
#' as length.               
#' 
#' 
#' @examples 
#' # see plotRankMatrixAsBar
#' @family generate_plot_data, benchmark
#' @export

generateRankMatrixAsBarData = function(bmr, measure = NULL, pos = "tile",
                                       order.lrns = NULL, order.tsks = NULL){
  # Assert Classes and fill NULL
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure)) 
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertChoice(pos,c("tile", "stack", "dodge"))
  
  
  # melt back into plotable form:
  df = convertBMRToRankMatrix(bmr, measure, ties.method = "random")
  df = melt(df, id.vars =c("learner.id"),
                       value.name = c("Rank"),
                       variable.name = c("task.id"))
  colnames(df) = c("learner.id","task.id","Rank")
  
  # Order if needed
  if (!is.null(order.lrns))
    df = orderBMRLrns( bmr, df, order.lrns)
  if (!is.null(order.tsks))
    df = orderBMRTasks(bmr, df, order.tsks)
  

  out = list("data" = df,
             "pos" = pos,
             "measure" = measure)
  
  class(out) = append(class(out), "RankMatrixAsBarData")
  return(out)
}



#' @title Plot a rank-matrix as a barplot.
#' 
#' @description Plots a barchart from the ranks of algorithms. Alternatively
#'  tiles can be plotted for every rank-task combination, see \code{pos}
#'  for details. The x-axis accross all plots is the ranks of a learner.id.
#'  Areas are always coloured corresponding to the learner.  
#'  
#' @return [\link{ggplot2}] plot
#' 
#' @param obj [\code{\link{RankMatrixAsBarData}}]\cr
#'  Output of a \code{\link{generateRankMatrixAsBarData}} function.
#' @param pos [\code{character(1)}]
#'  Optionally set how the bars are positioned in [\link{ggplot2}]. \cr
#'  Overwrites the one created in \link{generateRankMatrixAsBar}. \cr
#'  \code{"tile"} plots a heatmap with \link{task} as the y-axis.\cr
#'  Allows identification of the performance in a special task.\cr
#'  \code{"stack"} plots a stacked barplot. \cr
#'  Allows for comparison of learners within and and accross ranks.\cr
#'  \code{"dodge"} plots a barplot with bars next to each other instead
#'  of stacked bars.\cr
#' 
#' @examples 
#'  # see benchmark
#'  g = generateRankMatrixAsBarData(res, acc, order.Tsks = c(3L, 2L, 1L))
#'  plotRankMatrixAsBar(g, "tile")
#'  plotRankMatrixAsBar(g, "dodge")
#' 
#' @export

plotRankMatrixAsBar = function(obj, pos = NULL) {
  
  assertClass(obj, "RankMatrixAsBarData")
  if (is.null(pos))
    pos = obj$pos
  assertChoice(pos,c("tile","stack","dodge"))

  if (pos == "tile") {
    p = ggplot(obj$data) + 
      geom_tile(aes(x = as.factor(Rank),fill = learner.id, y = task.id),
                colour = "dimgrey", size  = 0.3) +
      xlab("Rank")
  } else if (pos != "tile") {
    p = ggplot(obj$data) + 
      geom_bar(aes(x = as.factor(Rank), fill = learner.id), position = pos) +
      xlab("Rank") +
      ylab(NULL)
  }
  return(p)
}
