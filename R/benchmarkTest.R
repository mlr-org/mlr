#' @title Perform overall Friedman test for a BenchmarkResult.
#'
#' @description Performs a \code{\link{friedman.test}} for a selected measure. \cr
#' The null hypothesis is that apart from an effect of the different
#' [\code{tasks}], the location parameter (aggregated performance-measure)
#' is the same for each \code{learner}.
#'
#' @template arg_bmr
#' @template arg_measure
#' @template arg_aggregation_method
#' @return A list of class \code{htest}. \cr
#' See \code{\link{friedman.test}} for details.\cr
#'
#' @examples
#' lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
#' tasks = list(iris.task, sonar.task)
#' rdesc = makeResampleDesc("CV", iters = 2L)
#' res = benchmark(lrns, tasks, rdesc, acc)
#' friedmanTestBMR(res)
#'
#' @family benchmark
#' @export
friedmanTestBMR = function(bmr, measure = NULL, aggregation = "default") {

  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertChoice(aggregation, c("default", "mean"))

  # aggregate means over iterations
  if (aggregation == "mean") {
    df = as.data.frame(bmr)
    df = aggregate(df[[measure$id]], by = list(task.id = df$task.id, learner.id = df$learner.id), FUN = mean)
  } else {
    aggr.meas = measureAggrName(measure)
    df = getBMRAggrPerformances(bmr, as.df = TRUE)
    df = df[, c("task.id", "learner.id", aggr.meas)]
    names(df)[names(df) == aggr.meas] = "x"
  }
  # Test
  tst = friedman.test(x ~ learner.id | task.id, data = df)
  return(tst)
}
#' @title Perform a posthoc Friedman-Nemenyi test.
#'
#' @description
#' Performs a \code{\link[PMCMR]{posthoc.friedman.nemenyi.test}} for a
#' \code{\link{BenchmarkResult}} and a selected measure.\cr
#' This means \code{all pairwise comparisons} of \code{learners} are performed.
#' The null hypothesis of the post hoc test is that each pair of learners is equal.
#' If the null hypothesis of the included ad hoc \code{\link{friedman.test}}
#' can be rejected a \code{pairwise.htest} is returned. If not, the function returns the
#' corresponding \link{friedman.test}
#'
#' @template arg_bmr
#' @template arg_measure
#' @param p.value [\code{numeric(1)}] \cr
#'   p-value for the tests.\cr  Default: 0.05
#' @template arg_aggregation_method
#' @return A list of class \code{pairwise.htest}.\cr See
#' \code{\link[PMCMR]{posthoc.friedman.nemenyi.test}} for details. \cr
#' Additionally two components are added to the list: \cr
#' \item{f.rejnull}{[\code{logical(1)}]whether the according friedman.test rejects the Null
#' hypothesis at the selected p.value}
#' \item{crit.difference}{[\code{list(2)}]Minimal difference the mean ranks of two learners
#' need to have in order to be significantly different}
#'
#' @examples
#' lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
#' tasks = list(iris.task, sonar.task)
#' rdesc = makeResampleDesc("CV", iters = 2L)
#' res = benchmark(lrns, tasks, rdesc, acc)
#' friedmanPostHocTestBMR(res, acc, p.value = 0.1)
#'
#' @family benchmark
#' @export
friedmanPostHocTestBMR = function(bmr, measure = NULL, p.value = 0.05, aggregation = "default") {
  requirePackages("PMCMR")
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertNumeric(p.value, lower = 0, upper = 1, len = 1)
  assertChoice(aggregation, c('default', 'mean'))

  n.learners = length(bmr$learners)
  if (n.learners < 2)
    message("Only one Learner to compare")
  assertNumeric(n.learners, lower = 2)
  n.tasks = length(bmr$results)

  # aggregate over iterations
  if (aggregation == "mean") {
    df = as.data.frame(bmr)
    df = aggregate(df[[measure$id]],
                   by = list(task.id = df$task.id,
                             learner.id = df$learner.id),
                   FUN = mean)
  } else if (aggregation == "default") {
    aggr.meas = measureAggrName(measure)
    df = getBMRAggrPerformances(bmr, as.df = TRUE)
    df = df[, c("task.id", "learner.id", aggr.meas)]
    names(df)[names(df) == aggr.meas] = "x"
  }

  f.test = friedmanTestBMR(bmr, measure)
  f.rejnull = f.test$p.value < p.value
  if (!f.rejnull)
    message("Cannot reject null hypothesis of overall Friedman test, returning overall Friedman test")
  # calculate critical difference(s)
  q.nemenyi = qtukey(1 - p.value, n.learners, 1e+06) / sqrt(2L)
  cd.nemenyi = q.nemenyi * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))
  q.bd = qtukey(1L - (p.value / (n.learners - 1L)), 2L, 1e+06) / sqrt(2L)
  cd.bd = q.bd * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))

  if (f.rejnull) {
    nem.test = PMCMR::posthoc.friedman.nemenyi.test(x ~ learner.id | task.id, data = df)
    nem.test$crit.difference = list("nemenyi" = cd.nemenyi, "bd" = cd.bd)
    nem.test$f.rejnull = f.rejnull
    return(nem.test)
  } else {
    f.test$f.rejnull = f.rejnull
    f.test$crit.difference = list("nemenyi" = cd.nemenyi, "bd" = cd.bd)
    return(f.test)
  }
}
