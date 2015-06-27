#' @title Create a Trellis-plot for a selected \link{Measure}
#' 
#' @description 
#' Plots Boxplots for a selected \code{measure} accross all iterations
#' of the resampling strategy, faceted by the \code{task.id}
#' 
#' @return [\link{ggplot2}] plot
#' 
#' @details Credit: This plot is similar to the one described in 
#' Eugster, J.A. (2012) 
#' 
#' @param bmr [\code{\link{BenchmarkResult}}] \cr
#'  Output of a \code{\link{benchmark}} function.
#' @param measure [\code{\link{Measure}}] \cr
#'  Measure for which ranks should be calculated (e.g: acc).
#'  Defaults to first. 
#' @param style [\code{character(1)]}]\cr
#'  Type of Plot, can be \code{"box"} for a boxplot or \code{"violin"} for
#'  a violin-plot.
#' @param order.lrns [\code{character(n.learners)}] or \cr 
#'                  [\code{integer(n.learners)}] \cr
#' Character vector with \code{learner.ids} in new order , or integer
#' vector refering to the positions in the new order.
#' @param order.tsks [\code{character(n.tasks)}] or \cr 
#'                   [\code{integer(n.tasks)}] \cr
#' Character vector with \code{task.ids} in new order, or an integer
#' vector refering to the positions in the new order.
#' 
#' @examples 
#' # see benchmark function
#' plotBenchmarkResult(res,acc)
#' 
#' @family plot, benchmark
#' @export


plotBenchmarkResult = function(bmr, measure = NULL, style = "box", 
                               order.lrns = NULL, order.tsks = NULL) {
  
  # Assertions
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertClass(style, "character")
  assertChoice(style, c("box", "violin"))
  
  # get and modify data.frame
  df = as.data.frame(bmr)
  if (!is.null(order.Lrns))
    df = orderBMRLrns(bmr, df, order.lrns)
  if (!is.null(order.Tsks))
    df = orderBMRTasks(bmr, df, order.tsks)
  
  # Create the plot
  p = ggplot(df, aes_string(x = "learner.id", y = measure$id,
                           color = "learner.id")) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = -45, hjust = 0)) +
    facet_wrap(~task.id)
  
  if (style == "box") {
    p = p + geom_boxplot() 
  }else if (style == "violin") {
    p = p + 
      geom_violin() + 
      stat_summary(fun.ymin = median, fun.ymax = median,
                   fun.y = median, geom = "crossbar",
                   color = "darkgrey", size = 0.4)
  }
  return(p)
}
