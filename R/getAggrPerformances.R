#' Extract the aggregated measures of a benchmark result.
#'
#' @template arg_bmr
#' @return [\code{data.frame}].
#' @export
#' @family benchmark
getAggrPerformances = function(object) {
  UseMethod("getAggrPerformances")
}

#' @export
getAggrPerformances.BenchmarkResult = function(object) {
  task.names = names(object)
  learner.names = unname(lapply(object, names))
  df = data.frame(
    task = rep.int(task.names, viapply(learner.names, length)),
    learner = unlist(learner.names),
    stringsAsFactors = FALSE
  )
  aggr = rowLapply(df, function(x) t(object[[x$task]][[x$learner]]$aggr))
  cbind(df, do.call(rbind, aggr))
}

