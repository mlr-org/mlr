#' @title Convert BenchmarkResult to a rank-matrix.
#' 
#' @description Computes a matrix of all the ranks of different algorithms
#'  over different datasets (tasks). Ranks are computed from aggregated 
#'  measures.
#' 
#' @return [\code{matrix}] \cr
#'  Matrix, with measure ranks as entries. \cr  
#'  The matrix has one row for each \code{learner},
#'  and one column for each \code{task}.
#' 
#' @param bmr [\code{\link{BenchmarkResult}}] \cr
#'  Output of a \code{\link{benchmark}} function.
#' @param measure [\code{\link{Measure}}] \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.
#' @param aggregation [\code{character(1)}] \cr
#'  Aggregation method for resampling strategy. \cr
#'  Can be \code{default} or \code{mean}.
#'  See \code{\link{getAggrPerformances}}. for details on \code{default}.
#' @param ties.method [\code{character(1)}]\cr
#'  see \code{\link{rank}} for details.
#' 
#' 
#' @examples 
#' # see benchmark
#' convertBMRToRankMatrix(res, acc)
#' 
#' @family convertBMR, benchmark
#' @export

convertBMRToRankMatrix = function(bmr, measure = NULL, ties.method = "average",
                                  aggregation = "default") {
  
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
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
  # due to a bug in plyr, eval(parse()) has to be called in order to 
  # dynamically adjust the ties.method
  if (!measure$minimize)
    df$x = desc(df$x)
  eval(parse(text = paste0(
    "df = ddply(df, .(task.id), transform, alg.rank = rank(x, ties.method = '",
    ties.method, "'))"
  )))
  
  # convert into matrix, rows = leaner, cols = tasks
  df = melt(df, c("task.id", "learner.id"), "alg.rank")
  df = dcast(df, learner.id ~ task.id )
  rownames(df) = df$learner.id
  mat = as.matrix(df[,colnames(df) != "learner.id"])
  
  return(mat)
}
