#' Returns the optimal hyperparameters and optimization path after training or benchmarking.
#'
#' @param object [\code{\link{WrappedModel}} | \code{BenchmarkResult}]\cr
#'   Trained Model created with \code{\link{makeTuneWrapper}} or benchmark result created with \code{\link{benchmark}}.
#' @return [\code{\link{TuneResult}} or list of \code{\link{TuneResult}}s].
#'   \code{NULL}, if no tuning was performed.
#' @family tune
#' @family benchmark
#' @export
getTuneResult = function(object) {
  UseMethod("getTuneResult")
}

#' @export
getTuneResult.WrappedModel = function(object) {
  if (inherits(object, "TuneModel"))
    object$learner.model$opt.result
  else
    NULL
}

#' @export
getTuneResult.BenchmarkResult = function(object) {
  getExtract(object, "TuneResult")
}

