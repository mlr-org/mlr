#' Returns the selected feature set and optimization path after training or benchmarking.
#'
#' @param object [\code{\link{WrappedModel}} | \code{BenchmarkResult}]\cr
#'   Trained Model created with \code{\link{makeFeatSelWrapper}} or benchmark result created with \code{\link{benchmark}}.
#' @return [\code{\link{FeatSelResult}} or list of \code{\link{FeatSelResult}}s].
#' @export
#' @family featsel
#' @family benchmark
getFeatSelResult = function(object) {
  UseMethod("getFeatSelResult")
}

#' @export
getFeatSelResult.WrappedModel = function(object) {
  object$learner.model$opt.result
}

#' @export
getFeatSelResult.BenchmarkResult = function(object) {
  getExtract(object, "FeatSelResult")
}


