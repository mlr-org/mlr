#' Returns a filter result after training or benchmarking.
#'
#' @param object [\code{\link{WrappedModel}} | \code{\link{BenchmarkResult}}]\cr
#'   Trained Model created with \code{\link{makeFilterWrapper}} or benchmark result created with \code{\link{benchmark}}.
#' @return [\code{\link{FilterResult}} or list of \code{\link{FilterResult}}s].
#' @aliases FilterResult
#' @export
#' @family filter
#' @family benchmark
getFilterResult = function(object) {
  UseMethod("getFilterResult")
}

#' @export
getFilterResult.WrappedModel = function(object) {
  x = getFilteredFeatures(object)
  # FIXME: what is this?
  if (is.null(x))
    return(NULL)
  addClasses(x, "FilterResult")
}

#' @export
getFilterResult.BenchmarkResult = function(object) {
  getExtract(object, "FilterResult")
}
