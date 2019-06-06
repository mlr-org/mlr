#' @title Perform a posthoc Friedman-Nemenyi test.
#'
#' @description
#' Performs a [PMCMR::posthoc.friedman.nemenyi.test] for a
#' [BenchmarkResult] and a selected measure.
#' This means *all pairwise comparisons* of `learners` are performed.
#' The null hypothesis of the post hoc test is that each pair of learners is equal.
#' If the null hypothesis of the included ad hoc [stats::friedman.test]
#' can be rejected an object of class `pairwise.htest` is returned. If not, the function returns the
#' corresponding \link[stats]{friedman.test}.
#' Note that benchmark results for at least two learners on at least two tasks
#' are required.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param p.value (`numeric(1)`)\cr
#'   p-value for the tests. Default: 0.05
#' @template arg_aggregation_method
#' @return ([pairwise.htest]): See [PMCMR::posthoc.friedman.nemenyi.test] for details.
#' Additionally two components are added to the list:
#' \describe{
#'   \item{f.rejnull (`logical(1)`)}{Whether the according friedman.test rejects the Null hypothesis at the selected p.value}
#'   \item{crit.difference (`list(2)`)}{Minimal difference the mean ranks of two learners need to have in order to be significantly different}
#' }
#'
#' @family benchmark
#' @noMd
#' @export
#' @examples
#' # see benchmark
friedmanPostHocTestBMR = function(bmr, measure = NULL, p.value = 0.05, aggregation = "default") {

  requirePackages("PMCMR")
  assertClass(bmr, "BenchmarkResult")
  assertNumeric(p.value, lower = 0, upper = 1, len = 1)
  assertChoice(aggregation, c("default", "mean"))
  measure = checkBMRMeasure(measure, bmr)
  n.learners = length(bmr$learners)
  if (n.learners < 2) {
    stop("Benchmark results for at least two learners are required")
  }
  n.tasks = length(bmr$results)
  if (n.tasks < 2) {
    stop("Benchmark results for at least two tasks are required")
  }

  # aggregate over iterations
  if (aggregation == "mean") {
    df = as.data.frame(bmr)
    df = aggregate(df[[measure$id]],
      by = list(task.id = df$task.id, learner.id = df$learner.id),
      FUN = mean)
    aggr.meas = "x"
  } else if (aggregation == "default") {
    aggr.meas = measureAggrName(measure)
    df = getBMRAggrPerformances(bmr, as.df = TRUE)
  }
  # Perform Friedman Test
  f.test = friedmanTestBMR(bmr, measure)
  if (!is.na(f.test$p.value)) {
    f.rejnull = f.test$p.value < p.value
    if (!f.rejnull) {
      warning("Cannot reject null hypothesis of overall Friedman test,
             returning overall Friedman test.")
    }
  } else {
    f.rejnull = FALSE
    warning("P-value not computable. Learner performances might be exactly equal.")
  }

  # calculate critical difference(s)
  q.nemenyi = qtukey(1 - p.value, n.learners, 1e+06) / sqrt(2L)
  cd.nemenyi = q.nemenyi * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))
  q.bd = qtukey(1L - (p.value / (n.learners - 1L)), 2L, 1e+06) / sqrt(2L)
  cd.bd = q.bd * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))

  if (f.rejnull) {
    form = as.formula(stri_paste(aggr.meas, " ~ learner.id | task.id", sep = ""))
    nem.test = PMCMR::posthoc.friedman.nemenyi.test(form, data = df)
    nem.test$crit.difference = list("nemenyi" = cd.nemenyi, "bd" = cd.bd)
    nem.test$f.rejnull = f.rejnull
    return(nem.test)
  } else {
    f.test$f.rejnull = f.rejnull
    f.test$crit.difference = list("nemenyi" = cd.nemenyi, "bd" = cd.bd)
    return(f.test)
  }
}
