#' @title Generate Data for RankMatrix as Barplot
#' 
#' @description Generate Data for \link{plotRankMatrixAsBar}.
#'  
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  \link{Measure} for which ranks should be calculated (e.g: acc). \cr
#'  Defaults to first.
#' @param pos [\code{character(1)}]
#' Optionally set how the bars are positioned in [\link{ggplot2}]. \cr
#'  \code{"tile"} plots a heatmap with \link{task} as the y-axis.\cr
#'  Allows identification of the performance in a special task.\cr
#'  \code{"stack"} plots a stacked barplot. \cr
#'  Allows for comparison of learners within and and accross ranks.\cr
#'  \code{"dodge"} plots a barplot of with dodged instead of stacked bars.\cr 
#' @param order.Lrns [\code{character(nLearners)}] or \cr 
#'                  [\code{integer(nLearners)}] \cr
#' Character vector with \code{learner.ids} in new order , or integer
#' vector refering to the positions in the new order.
#' @param order.Tsks [\code{character(nTasks)}] or \cr 
#'                  [\code{integer(nTasks)}] \cr
#' Character vector with \code{task.ids} in new order, or an integer
#' vector refering to the positions in the new order.                 
#' 
#' @return [\code{RankMatrixAsBarData}] \cr
#' List which contains the following info: \cr
#' $\code{data}: \code{data.frame} containing the data for plotting\cr
#' $\code{measure}: \link{Measure} the ranks are calculated on\cr
#' $\code{pos}: Positioning info for the plot\cr
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task, sonar.task, pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' # Tasks sortet Backwards:
#' generateRankMatrixAsBarData(res, acc, order.Tsks = c(3L,2L,1L))
#' @family generate_plot_data
#' @export

generateRankMatrixAsBarData = function(bmr, measure = NULL, pos = "tile",
                                       order.Lrns = NULL, order.Tsks = NULL){
  # Assert Classes and fill NULL
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure)) 
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertChoice(pos,c("tile", "stack", "dodge"))
  
  
  # Melt back into plotable form:
  df = convertBMRToRankMatrix(bmr, measure)
  df = melt(df, id.vars =c("learner.id"),
                       value.name = c("Rank"),
                       variable.name = c("task.id"))
  
  # Order if needed
  if (!is.null(order.Lrns))
    df = orderBMRLrns( bmr, df, order.Lrns)
  if (!is.null(order.Tsks))
    df = orderBMRTasks(bmr, df, order.Tsks)
  

  out = list("data" = df,
             "pos" = pos,
             "measure" = measure)
  
  class(out) = append(class(out), "RankMatrixAsBarData")
  return(out)
}



#' @title Plot Rankmatrix as Barplot
#' 
#' @description Plots a barchart from the ranks of algorithms. Alternatively
#'  tiles can be plotted for every rank-task combination, see \code{pos}
#'  for details. The x-axis accross all plots is the ranks of a learner.id.
#'  Areas are always coloured corresponding to the learner.  
#'  
#' 
#' @param obj [\code{RankMatrixAsBarData}]\cr
#'  Output of a \link{generateRankMatrixAsBarData} function.
#' @param pos [\code{character(1)}]
#' Optionally set how the bars are positioned in [\link{ggplot2}]. \cr
#' Overwrites the one created in \link{generateRankMatrixAsBar}. \cr
#'  \code{"tile"} plots a heatmap with \link{task} as the y-axis.\cr
#'  Allows identification of the performance in a special task.\cr
#'  \code{"stack"} plots a stacked barplot. \cr
#'  Allows for comparison of learners within and and accross ranks.\cr
#'  \code{"dodge"} plots a barplot of with dodged instead of stacked bars.\cr
#' 
#' @return [\link{ggplot2}] plot
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task, sonar.task, pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' d = generateRankMatrixAsBarData(res, acc, order.Tsks = c(3L, 2L, 1L))
#' plotRankMatrixAsBar(d, "tile")
#' plotRankMatrixAsBar(d, "dodge")
#' 
#' @export

plotRankMatrixAsBar = function(obj, pos = NULL) {
  
  # Assert correct input
  assertClass(obj, "RankMatrixAsBarData")
  
  if (is.null(pos))
    pos = obj$pos
  df = obj$data

  # Plot
  if (pos == "tile") {
    p = ggplot(df) + 
      geom_tile(aes(x = as.factor(Rank),fill = learner.id, y = task.id),
                colour = "dimgrey", size  = 0.3) +
      xlab("Rank")
  } else if (pos != "tile") {
    p = ggplot(df) + 
      geom_bar(aes(x = as.factor(Rank), fill = learner.id), position = pos) +
      xlab("Rank") +
      ylab(NULL)
  }
  return(p)
}
