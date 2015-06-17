##########################################################################
#' @title Get Data for RankMatrix as Barplot
#' 
#' @description Get the \code{data.frame} to plot a RanMatrix as a Barplot.
#'  
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.

#' 
#' @return [\code{data.frame}]
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task,sonar.task,pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' getRankMatrixAsBarData(res,acc)
#' getRankMatrixAsBarData(res,acc)
#' 
#' @export
getRankMatrixAsBarData = function(bmr,measure){
  # Assert Classes
  assertClass(bmr, "BenchmarkResult")
  if (!is.null(measure)){
    assertClass(measure, "Measure")
  } else {
    measure = getBMRMeasures(bmr)[[1]]
  }
  # Melt back into plotable form:
  df = convertBMRToRankMatrix(bmr,measure)
  n2 = dim(df)[2]
  df = melt(df,id.vars =c("learner.id"),
                       value.name = c("Rank"),
                       variable.name =c("task.id"))
  return(df)
}




##########################################################################
#' @title Plot Rankmatrix as Barplot
#' 
#' @description Plots a barchart from the ranks of algorithms. Alternatively
#'  tiles can be plotted for every rank-task combination, see \code{pos}
#'  for details. The x-axis accross all plots is the ranks of a learner.
#'  Areas are always coloured corresponding to the learner.  
#'  
#'  @details 
#'  \code{'tile'} plots a heatmap with \link{task} as the y-axis.\cr
#'  It allows identification of the performance in a special task.\cr
#'  \code{'stack'} plots a stacked barplot. \cr
#'  It allows for comparison of learners within and and accross ranks.\cr
#'  \code{'dodge'} plots a barplot of with dodged instead of stacked bars.\cr
#'   
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.
#' @param pos [\code{character(1)}]
#' Optionally set how the bars are positioned in ggplot2. \cr
#' See \code{Details} for explanation.
#' @param orderLrns [\code{character(nLearners)}] \cr 
#'                  [\code{integer(nLearners)}] \cr
#' Character vector with learner.ids in new order , or integer
#' vector refering to the positions in the new order.
#' @param orderLrns [\code{character(nTasks)}] \cr 
#'                  [\code{integer(nTasks)}] \cr
#' Character vector with task.ids in new order , or integer
#' vector refering to the positions in the new order.
#' 
#' @return [\link{ggplot2}] plot
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task,sonar.task,pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' plotRankMatrixAsBar(res,acc,"dodge")
#' plotRankMatrixAsBar(res,acc,orderLrns = c(1,2,4,3),orderTsks=c(3,2,1))
#' 
#' @export

plotRankMatrixAsBar = function(bmr, measure = NULL,pos = "tile",
                               orderLrns = NULL, orderTsks = NULL){
  
  assertChoice(pos,c("tile","stack","dodge"))
  df = getRankMatrixAsBarData(bmr,measure)
  if (!is.null(orderLrns)){df = orderBMRLrns( bmr, df, orderLrns)}
  if (!is.null(orderTsks)){df = orderBMRTasks(bmr, df, orderTsks)}
  # Plot the data
  if (pos == "tile"){
    p = ggplot(df) + 
      geom_tile(aes(x=as.factor(Rank),fill = learner.id,y= task.id),
                colour = "dimgrey", size  = 0.3)+
      xlab("Rank")
  } else if (pos != "tile"){
    p = ggplot(df) + 
      geom_bar(aes(x=as.factor(Rank),fill = learner.id), position = pos)+
      xlab("Rank")+
      ylab(NULL)
  }
  return(p)
}
