#' @title Create a Trellis-plot for a selected measure.
#' 
#' @description 
#' Plots boxplots for a selected \code{measure} accross all iterations
#' of the resampling strategy, faceted by the \code{task.id}
#' 
#' @template arg_bmr
#' @template arg_measure
#' @param style [\code{character(1)]}]\cr
#'   Type of Plot, can be \dQuote{box} for a boxplot or \dQuote{violin} for
#'   a violin-plot.
#' @template arg_order_lrns
#' @template arg_order_tsks
#' @template ret_gg2
#' 
#' @references Manuel J. A. Eugster, Torsten Hothorn and Friedrich Leisch;
#' Domain-Based Benchmark Experiments:Exploratory and Inferential Analysis,
#' AUSTRIAN JOURNAL OF STATISTICS Volume 41 (2012), Number 1, 5–26,
#' but does not include any clustering or sorting.
#' @examples 
#' # see benchmark function
#' # plotBenchmarkResult(res,acc)
#' 
#' @family plot
#' @family benchmark
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
  if (!is.null(order.lrns))
    df = orderBMRLrns(bmr, df, order.lrns)
  if (!is.null(order.tsks))
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
