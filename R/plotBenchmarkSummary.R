#' @title Generate data for a Benchmark-summary plot.
#' 
#' @description
#'  A BenchmarkSummaryplot shows allows comparison of performance of different
#'  learners within a task.
#' 
#' @return \code{BenchmarkSummaryData}, contains: \cr
#'  $\code{data}: [\code{data.frame}]. \cr
#'  $\code{fill}: selected \code{fill} option.
#'  $\code{measure}: selected \code{measure}.
#'   
#' @param bmr [\code{\link{BenchmarkResult}}] \cr
#'  Output of a \code{\link{benchmark}} function.
#' @param measure [\code{\link{Measure}}] \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.
#' @param fill [\code{character(0)}] \cr 
#'  Fill bars proportional to [\code{fill}] \cr
#'  Can be \code{best} or \code{worst}.
#'  \code{"best"} compares all performances to the best performance accross all
#'  classifiers. Bars are filled proportional to the best performance. \cr
#'  \code{"worst"} compares all performances to the worst performance accross
#'  all classifiers. Bars are filled proportional to the worst performance. \cr
#' @param order.lrns [\code{character(n.learners)}] or \cr 
#'                   [\code{integer(n.learners)}] \cr
#'  Character vector with \code{learner.ids} in new order , or integer
#'  vector refering to the positions in the new order.
#' @param order.tsks [\code{character(n.tasks)}] or \cr 
#'                  [\code{integer(n.tasks)}] \cr
#'  Character vector with \code{task.ids} in new order, or an integer
#'  vector refering to the positions in the new order.
#' 
#' @examples 
#' # see plotBenchmarkSummary
#' 
#' @family generate_plot_data, benchmark
#' @export

generateBenchmarkSummaryData = function(bmr, measure = NULL, fill = "best",
                                        order.lrns = NULL, order.tsks = NULL) {
  
  assertClass(bmr, "BenchmarkResult")
  assertChoice(fill,c("worst","best"))
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  
  # aggregate data over iterations
  aggrMeas = measureAggrName(measure)
  df = getBMRAggrPerformances(bmr, as.df = TRUE)
  df = df[,c("task.id", "learner.id", aggrMeas)]
  names(df)[names(df) == aggrMeas] = c("x")
  
  if (!is.null(order.lrns))
    df = orderBMRLrns( bmr, df, orderlrns)
  if (!is.null(order.tsks))
    df = orderBMRTasks(bmr, df, ordertsks)

# get min / max performance in task
  row.names(df) = NULL
  df2 = df
  if (fill == "worst") {
    if (!measure$minimize) {
      df = ddply(df, .(task.id), mutate, x = (1 - x) / (1 - min(x)))
      df2$x = 1 - df$x
    } else if(measure$minimize) {
      df = ddply(df, .(task.id), mutate, x = x / max(x))
      df2$x = 1 - df$x
    }
  } else if (fill == "best") {
    if (!measure$minimize) {
      df = ddply(df, .(task.id), mutate, x = x / max(x))
      df2$x = 1 - df$x
    } else if (measure$minimize) {
      df = ddply(df, .(task.id), mutate, x = (1 - x) / (1 - min(x)))
      df2$x = 1 - df$x
    }
  }
   
  # rbind data.frames with different alphas
  df2$alpha = 0.4
  df$alpha  = 1
  df = rbind(df, df2)
  df = sortByCol(df, c("task.id", "learner.id"))
  
  out = list("data" = df,
             "fill" = fill,
             "measure" = measure)

  class(out) = append(class(out), "BenchmarkSummaryData")
  return(out)
}



#' @title plot a Summary for a Benchmarkresult
#' 
#' @description
#'  Plots a BenchmarkSummaryPlot for a selected \link{Measure}.
#'  Full tiles correspond
#'  to the worst performance accross all \code{tasks.ids}
#'  and \code{learner.ids}. The actuall fill
#'  corresponds to the proportional performance of the \code{best}
#'  or \code{worst} within the task.
#'  
#' @return \link{ggplot2}] plot 
#' 
#' @details 
#'  Credit: This plot is analogous to the one described in Eugster,J.A.(2012)
#'  but does not include any clustering or sorting.
#'   
#' @param obj [\code{BenchmarkSummaryData}]\cr
#'  Output of a \link{generateBenchmarkSummaryData} function.
#' 
#' 
#' @examples 
#' # see benchmark
#' g = generateBenchmarkSummaryData(res,ber,fill = "best")
#' plotBenchmarkSummary(g)
#' 
#' @family plot, benchmark
#' @export

plotBenchmarkSummary = function(obj) {
  
  assertClass(obj, "BenchmarkSummaryData")
  df = obj$data
  #Plot
  p = ggplot(df) +
    geom_bar(aes(x = task.id, y = x, fill = learner.id, alpha = alpha),
             stat = "identity", linetype = 1, color = "grey", size = .001) +
    scale_alpha_identity() +
    coord_flip() +
    ylab(paste(obj$measure$name, " proportional to", obj$fill, "performance")) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank())
  
  return(p)
}

