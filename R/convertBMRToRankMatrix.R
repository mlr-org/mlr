#' @title Convert BenchmarkResult to a Rankmatrix
#' 
#' @description Computes a matrix of all the ranks of different algorithms
#' over different datasets(tasks). Ranks are computed from aggregated 
#' measures. 
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.
#' @param aggregation [\code{character](1)}] \cr
#'  Aggregation method for resampling strategy. \cr
#'  Can be \code{default} or \code{mean}.
#'  See \link{getAggrPerformances} for details.
#' 
#' @return [\code{data.frame}] \cr
#' Matrix, with measure ranks as entries. \cr  
#' The columns of the matrix correspond to tasks, rows correspond to learners.
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task,sonar.task,pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' convertBMRToRankMatrix(res,acc)
#' 
#' @export

convertBMRToRankMatrix = function(bmr,measure = NULL,aggregation = "default"){
  #Assert class and convert to data.frame
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure)){
  measure = getBMRMeasures(bmr)[[1]]
  }
  assertClass(measure, "Measure")
  assertChoice(aggregation, c("mean","default"))
# Aggregate mean over iterations
  if (aggregation == "mean"){ 
  df = as.data.frame(bmr)
  df = aggregate(df[[measure$id]],
                 by = list(task.id = df$task.id,
                           learner.id = df$learner.id),
                 FUN= mean)
  } else if (aggregation == "default"){
  aggrMeas = mlr:::measureAggrName(measure)
  df = mlr::getBMRAggrPerformances(bmr,as.df = TRUE)
  df = df[,c("task.id","learner.id",aggrMeas)]
  names(df)[names(df)== aggrMeas] = c("x")
  }
  # Calculate Ranks, ties broken randomly, rank according to minimize option   
  # of selected measure
  if (measure$minimize == FALSE){
    df = ddply(df,.(task.id),mutate,AlgRank = rank(desc(x),
                                                   ties.method = "random"))
  } else if(measure$minimize == TRUE){
    df = ddply(df,.(task.id),mutate,AlgRank = rank(x,ties.method = "random"))
  }
  # melt/cast into matrix 
  df = melt(df,c("task.id","learner.id"),"AlgRank")
  df = dcast(df, learner.id ~ task.id )
  #Return data.frame
  return(df)
}
