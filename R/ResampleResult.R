#' @title ResampleResult object.
#'
#' @description A container for resample results.
#'
#' @details Resample Result:
#'
#' A resample result is created by resample and
#' contains the following object members:
#' \describe{
#' \item{task.id (`character(1)`):}{
#'   Name of the Task.
#' }
#' \item{learner.id (`character(1)`):}{
#'   Name of the Learner.
#' }
#' \item{measures.test ([data.frame]):}{
#'   Gives you access to performance measurements
#'   on the individual test sets. Rows correspond to sets in resampling iterations,
#'   columns to performance measures.
#' }
#' \item{measures.train ([data.frame]):}{
#'   Gives you access to performance measurements
#'   on the individual training sets. Rows correspond to sets in resampling iterations,
#'   columns to performance measures. Usually not available, only if specifically requested,
#'   see general description above.
#' }
#' \item{aggr ([numeric]):}{
#'   Named vector of aggregated performance values. Names are coded like
#'   this `<measure>.<aggregation>`.
#' }
#' \item{err.msgs ([data.frame]):}{
#'   Number of rows equals resampling iterations
#'   and columns are: `iter`, `train`, `predict`.
#'   Stores error messages generated during train or predict, if these were caught
#'   via [configureMlr].
#' }
#' \item{err.dumps (list of list of [dump.frames]):}{
#'   List with length equal to number of resampling iterations. Contains lists
#'   of `dump.frames` objects that can be fed to `debugger()` to inspect
#'   error dumps generated on learner errors. One iteration can generate more than
#'   one error dump depending on which of training, prediction on training set,
#'   or prediction on test set, operations fail. Therefore the lists have named
#'   slots `$train`, `$predict.train`, or `$predict.test` if relevant.
#'   The error dumps are only saved when option `on.error.dump` is `TRUE`.
#' }
#' \item{pred ([ResamplePrediction]):}{
#'   Container for all predictions during resampling.
#' }
#' \item{models [list of [WrappedModel]):}{
#'   List of fitted models or `NULL`.
#' }
#' \item{extract ([list]):}{
#'   List of extracted parts from fitted models or `NULL`.
#' }
#' \item{runtime (`numeric(1)`):}{
#'   Time in seconds it took to execute the resampling.
#' }
#' }
#' The print method of this object gives a short overview, including
#' task and learner ids, aggregated measures and runtime for the resampling.
#' @name ResampleResult
#' @rdname ResampleResult
#' @family resample
#' @family debug
NULL

#' @export
print.ResampleResult = function(x, ...) {
  cat("Resample Result\n")
  catf("Task: %s", x$task.id)
  catf("Learner: %s", x$learner.id)
  catf("Aggr perf: %s", perfsToString(x$aggr))
  catf("Runtime: %g", x$runtime)
  invisible(NULL)
}
