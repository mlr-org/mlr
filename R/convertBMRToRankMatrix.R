#' @title Convert BenchmarkResult to a rank-matrix.
#'
#' @description Computes a matrix of all the ranks of different algorithms
#' over different datasets (tasks). Ranks are computed from aggregated
#' measures.
#' Smaller ranks imply better methods, so for measures that are minimized, small ranks imply small scores.
#' for measures that are maximized, small ranks imply large scores.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param ties.method (`character(1)`)\cr
#'   See [base::rank] for details.
#' @template arg_aggregation_method
#' @return ([matrix]) with measure ranks as entries.
#'   The matrix has one row for each `learner`, and one column for each `task`.
#' @family benchmark
#' @export
#' @examples
#' # see benchmark
convertBMRToRankMatrix = function(bmr, measure = NULL, ties.method = "average", aggregation = "default") {

  assertClass(bmr, "BenchmarkResult")
  measure = checkBMRMeasure(measure, bmr)
  assertChoice(aggregation, c("mean", "default"))

  # aggregate mean over iterations
  if (aggregation == "mean") {
    df = setDT(as.data.frame(bmr))
    df = df[, list(x = mean(get(measure$id))), by = c("task.id", "learner.id")]
  } else if (aggregation == "default") {
    aggr.meas = measureAggrName(measure)
    df = setDT(getBMRAggrPerformances(bmr, as.df = TRUE))
    df = df[, c("task.id", "learner.id", aggr.meas), with = FALSE]
    setnames(df, aggr.meas, "x")
  }

  # calculate ranks, rank according to minimize option of the measure
  if (!measure$minimize) {
    df$x = -df$x
  }
  df[, "alg.rank" := rank(.SD$x, ties.method = ties.method), by = "task.id"] # nolint FIXME: find out what `:=` looks like in the AST and adjust the linter

  # convert into matrix, rows = leaner, cols = tasks
  df = melt(df, c("task.id", "learner.id"), "alg.rank")
  df = dcast(df, learner.id ~ task.id)
  task.id.names = setdiff(colnames(df), "learner.id")
  mat = as.matrix(setDF(df)[, task.id.names])
  rownames(mat) = df$learner.id
  colnames(mat) = task.id.names
  return(mat)
}
