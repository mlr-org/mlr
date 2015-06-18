#' @title Perform Friedman-Test on BenchmarkResult
#' 
#' @description Performs a Friedman Test for a selected measure. \cr
#' The null hypothesis is that apart from an effect of the task, the
#' location parameter (aggregated Performance measure) is the same 
#' for each learner.
#'  
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.
#' @param aggregation [\code{character](1)}] \cr
#'  Aggregation method for resampling strategy. \cr
#'  Can be \code{default} or \code{mean}.\cr
#'  See \link{getAggrPerformances} for details.
#' 
#' @return A list of class \code{htest}. \cr
#' See \link[stats]{friedman.test} for details.\cr
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task,sonar.task,pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' friedmanTestBMR(res, measure)
#' 
#' @family htest
#' @export


friedmanTestBMR = function(bmr, measure = NULL, aggregation = "default") {
  
  #Assert correct inputs
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertChoice(aggregation, c("default", "mean"))
  
  # Aggregate means over iterations
  if (aggregation == "mean") { 
    df = as.data.frame(bmr)
    df = aggregate(df[[measure$id]],
                   by = list(task.id = df$task.id,
                             learner.id = df$learner.id),
                   FUN = mean)
  } else if (aggregation == "default") {
    aggrMeas = measureAggrName(measure)
    df = getBMRAggrPerformances(bmr, as.df = TRUE)
    df = df[,c("task.id", "learner.id",aggrMeas)]
    names(df)[names(df) == aggrMeas] = c("x")
  }
  #Test
  tst = friedman.test(x ~ learner.id | task.id, data = df)
  return(tst)
}
