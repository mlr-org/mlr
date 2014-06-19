#' Returns the optimal hyperparameters and optimization path after training or benchmarking.
#'
#' @param object [\code{\link{WrappedModel}} | \code{BenchmarkResult}]\cr
#'   Trained Model created with \code{\link{makeTuneWrapper}} or benchmark result created with \code{\link{benchmark}}.
#' @return [\code{\link{TuneResult}} or list of \code{\link{TuneResult}}s].
#' @family tune
#' @family benchmark
#' @export
getTuneResult = function(object) {
  UseMethod("getTuneResult")
}

#' @export
getTuneResult.WrappedModel = function(object) {
  object$learner.model$opt.result
}

#' @export
getTuneResult.BenchmarkResult = function(object) {
  getExtract(object, "TuneResult")
}

