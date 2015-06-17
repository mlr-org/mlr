##########################################################################
#' @title Create a Trellis-plot for a measure
#' 
#' @description 
#' Plots Boxplots for a selected measure accross all iterations of an algorithm
#' faceted by the task.id
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc).
#'  Defaults to first. 
#' @param style [\code{character(1)]}]\cr
#'  Type of Plot, can be \code{"box"} for a boxplot or \code{"violin"} for
#'  a violin-plot.
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
#' plotBenchmarkResult(res,acc)
#' 
#' @export


plotBenchmarkResult = function(bmr,measure= NULL,style= "box",orderLrns = NULL,
                               orderTsks = NULL){
  
  assertClass(bmr, "BenchmarkResult")
  if (!is.null(measure)){
    assertClass(measure, "Measure")
  } else {
    measure = getBMRMeasures(bmr)[[1]]
  }
  assertClass(style,"character")
  assertChoice(style,c("box","violin"))
  df = as.data.frame(bmr)
  if (!is.null(orderLrns)){df = orderBMRLrns( bmr, df, orderLrns)}
  if (!is.null(orderTsks)){df = orderBMRTasks(bmr, df, orderTsks)}
  p = ggplot(df,aes_string(x = "learner.id",y=measure$id,color="learner.id"))+
    theme(axis.title.x = element_blank()) +
    facet_wrap(~task.id)
  if(style == "box"){
    p = p + geom_boxplot() 
  }else if(style == "violin"){
    p = p + geom_violin() + stat_summary(fun.ymin = median, fun.ymax = median,
                                         fun.y = median, geom="crossbar",
                                         color="darkgrey", size = 0.4)
  }
  return(p)
}
