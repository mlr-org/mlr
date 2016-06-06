#' @title Perform overall Friedman test for a BenchmarkResult.
#'
#' @description Performs a \code{\link[stats]{friedman.test}} for a selected measure.
#' The null hypothesis is that apart from an effect of the different
#' [\code{\link{Task}}], the location parameter (aggregated performance-measure)
#' is the same for each \code{\link{Learner}}.
#'
#' @template arg_bmr
#' @template arg_measure
#' @template arg_aggregation_method
#' @return [\code{htest}]: See \code{\link[stats]{friedman.test}} for details.
#' @family benchmark
#' @export
#' @examples
#' # see benchmark
friedmanTestBMR = function(bmr, measure = NULL, aggregation = "default") {
  
  assertClass(bmr, "BenchmarkResult")
  measure = checkBMRMeasure(measure, bmr)
  assertChoice(aggregation, c("default", "mean"))
  
  # aggregate mean or default over iterations
  if (aggregation == "mean") {
    df = as.data.frame(bmr)
    df = aggregate(df[[measure$id]],
                   by = list(task.id = df$task.id, learner.id = df$learner.id),
                   FUN = mean)
    aggr.meas = "x"
  } else {
    aggr.meas = measureAggrName(measure)
    df = getBMRAggrPerformances(bmr, as.df = TRUE)
  }
  friedman.test(as.formula(paste(aggr.meas, " ~ learner.id | task.id", sep = "")), data = df)
}
