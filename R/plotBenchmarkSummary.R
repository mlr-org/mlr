##########################################################################
#' @title Get data for a BenchmarkSummary plot.
#' 
#' @description
#' Get dataset used to plot a BenchmarkSummary plot.
#' @details 
#' 'best' compares all performances to the best performance accross all \cr
#'  classifiers. Bars are filled proportional to the best performance. \cr
#' 'worst' compares all performances to the worst performance accross all \cr
#'  classifiers. Bars are filled proportional to the worst performance. \cr
#'   
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). \cr
#'  Defaults to first.
#' @param fill [\code{character(0)}] \cr 
#'  Fill bars proportional to [\code{fill}] \cr
#'  Can be \code{best} or \code{worst}. See \code{Details}.
#' 
#' @return \code{data.frame}
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task,sonar.task,pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' getBenchmarkSummaryData(res,acc)
#' 
#' @export

getBenchmarkSummaryData = function(bmr,measure = NULL, fill = "best"){
  assertClass(bmr, "BenchmarkResult")
  assertChoice(fill,c("worst","best"))
  if (is.null(measure)){
    measure = getBMRMeasures(bmr)[[1]]
  }
  assertClass(measure, "Measure")
  df = as.data.frame(bmr)
  aggrMeas = mlr:::measureAggrName(measure)
  df = mlr::getBMRAggrPerformances(bmr,as.df = TRUE)
  df = df[,c("task.id","learner.id",aggrMeas)]
  names(df)[names(df)== aggrMeas] = c("x")
  # Calculate Positions: Create 2 dfs, one for actual value, other to fill up
  # to next task.
  df2 = df
  if (fill == "worst"){
    if (measure$minimize == FALSE){
      df$x = (1 - df$x)/(1-min(df$x))
      df2$x = 1-df$x
    } else if(measure$minimize == TRUE){
      df$x = df$x/max(df$x)
      df2$x = 1-df$x
    }
  } else if (fill == "best"){
    if (measure$minimize == FALSE){
      df$x = df$x/(max(df$x))
      df2$x = 1-df$x
    } else if (measure$minimize == TRUE){
      df$x = (1-df$x)/(1 - min(df$x))
      df2$x = 1-df$x
    }
  }
  #Create alpha
  df2$alpha = 0.4
  df$alpha  = 1
  #Fit together dfs with different alphas
  df = rbind(df,df2)
  df = sortByCol(df,c("task.id","learner.id"))
  return(df)
}




##########################################################################
#' @title plotBenchmarkSummary
#' 
#' @description
#' Plots a BenchmarkSummaryPlot for a selected measure. Full tiles correspond
#' to the worst performance accross all tasks and learners. The actuall fill
#' corresponds to the proportional performance of the learner in the task.
#' 
#' @details 
#' 'best' compares all performances to the best performance accross all \cr
#'  classifiers. Bars are filled proportional to the best performance. \cr
#' 'worst' compares all performances to the worst performance accross all \cr
#'  classifiers. Bars are filled proportional to the worst performance. \cr
#'  This plot is analogous to the one described in Eugster, J.A.(2012) \cr
#'  but does not include any clustering. 
#'   
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). \cr
#'  Defaults to first.
#' @param fill [\code{character(0)}] \cr 
#'  Fill bars proportional to [\code{fill}] \cr
#'  Can be \code{best} or \code{worst}. See \code{Details}.
#' @param orderLrns [\code{character(nLearners)}] \cr 
#'                  [\code{integer(nLearners)}] \cr
#' Character vector with learner.ids in new order , or integer
#' vector refering to the positions in the new order.
#' @param orderLrns [\code{character(nTasks)}] \cr 
#'                  [\code{integer(nTasks)}] \cr
#' Character vector with task.ids in new order , or integer
#' vector refering to the positions in the new order.
#' 
#' @return \link{ggplot2}] plot
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task,sonar.task,pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' plotBenchmarkSummary(res,acc,fill = "best")
#' 
#' @export

plotBenchmarkSummary = function(bmr,measure=NULL,fill = "best",
                                orderLrns = NULL, orderTsks = NULL){
  df = getBenchmarkSummaryData(bmr,measure,fill)
  if (!is.null(orderLrns)){df = orderBMRLrns( bmr, df, orderLrns)}
  if (!is.null(orderTsks)){df = orderBMRTasks(bmr, df, orderTsks)}
  #Plot
  p = ggplot(df)+
    geom_bar(aes(x=task.id,y=x, fill =learner.id,alpha =alpha),
             stat = "identity",linetype = 1, color = "grey",
             size = .001)+
    scale_alpha_identity()+
    coord_flip()+
    ylab(paste(measure$name," proportional to",fill,"performance"))+
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank())
  
  return(p)
}

