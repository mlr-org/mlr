#' @title Generate data for a BenchmarkSummary plot.
#' 
#' @description
#' Generate data used to plot a BenchmarkSummary plot.
#'   
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). \cr
#'  Defaults to first.
#' @param fill [\code{character(0)}] \cr 
#'  Fill bars proportional to [\code{fill}] \cr
#'  Can be \code{best} or \code{worst}.
#'  \code{"best"} compares all performances to the best performance accross all
#'  classifiers. Bars are filled proportional to the best performance. \cr
#' \code{"worst"} compares all performances to the worst performance accross
#'  all classifiers. Bars are filled proportional to the worst performance. \cr
#' @param order.Lrns [\code{character(nLearners)}] or \cr 
#'                  [\code{integer(nLearners)}] \cr
#' Character vector with \code{learner.ids} in new order , or integer
#' vector refering to the positions in the new order.
#' @param order.Tsks [\code{character(nTasks)}] or \cr 
#'                  [\code{integer(nTasks)}] \cr
#' Character vector with \code{task.ids} in new order, or an integer
#' vector refering to the positions in the new order.
#' 
#' @return \code{BenchmarkSummaryData}, contains: \cr
#' $\code{data}: [\code{data.frame}]. \cr
#' $\code{fill}: selected \code{fill} option.
#' $\code{measure}: selected \code{measure}.
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task, sonar.task, pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' generateBenchmarkSummaryData(res, acc)
#' 
#' @family generate_plot_data
#' @export

generateBenchmarkSummaryData = function(bmr, measure = NULL, fill = "best",
                                        order.Lrns = NULL, order.Tsks = NULL) {
  
  #Asser Correct Input
  assertClass(bmr, "BenchmarkResult")
  assertChoice(fill,c("worst","best"))
  if (is.null(measure)){
    measure = getBMRMeasures(bmr)[[1L]]
  }
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  
  # Get Data and Aggregate
  df = as.data.frame(bmr)
  aggrMeas = measureAggrName(measure)
  df = getBMRAggrPerformances(bmr, as.df = TRUE)
  df = df[,c("task.id", "learner.id", aggrMeas)]
  names(df)[names(df) == aggrMeas] = c("x")
  if (!is.null(order.Lrns))
    df = orderBMRLrns( bmr, df, order.Lrns)
  if (!is.null(order.Tsks))
    df = orderBMRTasks(bmr, df, order.Tsks)
  
  
  
  # Calculate Positions: Create 2 dfs, one for actual value, other to fill up
  # to next task.
  df2 = df
  if (fill == "worst") {
    if (measure$minimize == FALSE) {
      df$x = (1 - df$x) / (1 - min(df$x))
      df2$x = 1 - df$x
    } else if(measure$minimize == TRUE) {
      df$x = df$x / max(df$x)
      df2$x = 1 - df$x
    }
  } else if (fill == "best") {
    if (measure$minimize == FALSE) {
      df$x = df$x / (max(df$x))
      df2$x = 1 - df$x
    } else if (measure$minimize == TRUE) {
      df$x = (1 - df$x) / (1 - min(df$x))
      df2$x = 1 - df$x
    }
  }
  #Create alpha
  df2$alpha = 0.4
  df$alpha  = 1
  #Fit together dfs with different alphas
  df = rbind(df, df2)
  df = sortByCol(df, c("task.id", "learner.id"))
  
  out = list("data" = df,
             "fill" = fill,
             "measure" = measure)

  class(out) = append(class(out), "BenchmarkSummaryData")
  return(out)
  
}



#' @title plotBenchmarkSummary
#' 
#' @description
#' Plots a BenchmarkSummaryPlot for a selected \link{Measure}.
#' Full tiles correspond
#' to the worst performance accross all \code{tasks.ids}
#' and \code{learner.ids}. The actuall fill
#' corresponds to the proportional performance of the \code{best}
#' or \code{worst} algorithm accross all tasks.
#' 
#' @details 
#'  Credit: This plot is analogous to the one described in Eugster,J.A.(2012)
#'  but does not include any clustering. 
#'   
#' @param obj [\code{BenchmarkSummaryData}]\cr
#'  Output of a \link{generateBenchmarkSummaryData} function.
#' 
#' @return \link{ggplot2}] plot
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task, sonar.task, pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' g = generateBenchmarkSummaryData(res,ber,fill = "best")
#' plotBenchmarkSummary(g)
#' 
#' @family plot
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

