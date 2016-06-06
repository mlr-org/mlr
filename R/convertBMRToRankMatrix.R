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
#' @param ties.method [\code{character(1)}]\cr
#'   See \code{\link[base]{rank}} for details.
#' @template arg_aggregation_method
#' @return [\code{matrix}] with measure ranks as entries.
#'   The matrix has one row for each \code{learner}, and one column for each \code{task}.
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
    df = as.data.frame(bmr)
    df = aggregate(df[[measure$id]],
                   by = list(task.id = df$task.id,
                             learner.id = df$learner.id),
                   FUN = mean)
  } else if (aggregation == "default") {
    aggr.meas = measureAggrName(measure)
    df = getBMRAggrPerformances(bmr, as.df = TRUE)
    df = df[, c("task.id", "learner.id", aggr.meas)]
    names(df)[names(df) == aggr.meas] = c("x")
  }

  # calculate ranks, rank according to minimize option of the measure
  if (!measure$minimize)
    df$x = -df$x
  df = plyr::ddply(df, "task.id", function(d) {
    d$alg.rank = rank(d$x, ties.method = ties.method)
    return(d)
  })

  # convert into matrix, rows = leaner, cols = tasks
  df = reshape2::melt(df, c("task.id", "learner.id"), "alg.rank")
  df = reshape2::dcast(df, learner.id ~ task.id )
  rownames(df) = df$learner.id
  mat = as.matrix(df[,colnames(df) != "learner.id"])

  return(mat)
}
