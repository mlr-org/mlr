#' @title Perform a posthoc Nemenyi Test
#' 
#' @description
#' Performs a \link[PMCMR]{posthoc.friedman.nemenyi.test} for a 
#' \link{BenchmarkResult} and a selected measure.
#' The null hypothesis is that apart from an effect of the task, the location
#' parameter (mean of aggregated Perforamance Measure) is the same for 
#' each learner.
#' 
#' @details
#' If the Null Hypthesis of the included ad-hoc friedman.test can be rejected a
#' \code{pairwise.htest} is returned. If not, the function returns the 
#' corresponding \code{friedman.test}
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.
#'  @param p.value [\code{numeric}(1)] \cr
#'  P-value for the tests.\cr  Default: 0.05
#' 
#' @return A list of class "pairwise.htest". See 
#' \link[PMCMR]{posthoc.friedman.nemenyi.test} for details. \cr
#' Additionally two components are added to the list: \cr
#' \code{$fRejNull:} whether the according friedman.test rejects the Null
#' hypothesis.\cr
#' \code{$cDifference}: Minimal difference the mean ranks of two learners
#' need to have in order to be significantly different.
#' 
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task, sonar.task, pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' posthocNemenyiTestBMR(res, acc)
#' 
#' @export


posthocNemenyiTestBMR = function(bmr, measure = NULL, p.value = 0.05,                                                aggregation = 'default') {
  
  requirePackages("PMCMR")
  
  #Assert correct inputs
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertNumeric(p.value,lower = 0, upper = 1,len=1)
  assertChoice(aggregation,c('default','mean'))
  # Number of Learners/Tasks
  nLearners = length(bmr$learners)
  if (nLearners < 2)
     message("Only one Learner to compare")
  assertNumeric(nLearners, lower = 2)
  nTasks = length(bmr$results)
  # Take mean across iterations
  if (aggregation == "mean") { 
    df = as.data.frame(bmr)
    df = aggregate(df[[measure$id]],
                   by = list(task.id = df$task.id,
                             learner.id = df$learner.id),
                   FUN= mean)
  } else if (aggregation == "default") {
    aggrMeas = measureAggrName(measure)
    df = getBMRAggrPerformances(bmr, as.df = TRUE)
    df = df[,c("task.id", "learner.id", aggrMeas)]
    names(df)[names(df) == aggrMeas] = c("x")
  }
  fTest = friedmanTestBMR(bmr, measure)
  fRejNull = fTest$p.value < p.value
  
  #Calculate Critical Difference
  q  = qtukey(1 - (p.value / (nLearners - 1)), 2, 1e+06) / sqrt(2)
  CD = q * sqrt(nLearners * (nLearners + 1) / (6 * nTasks))
  if (fRejNull == TRUE) {
    nemTest = posthoc.friedman.nemenyi.test(x ~ learner.id | task.id, data = df)
    nemTest$cDifference = CD
    nemTest$fRejNull = TRUE
    return(nemTest)
  } else if (fRejNull == FALSE) {
    fTest$fRejNull = FALSE
    fTest$cDifference = CD
    return(fTest)
  }
}
