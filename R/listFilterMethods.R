#' Returns all available feature filter methods.
#'
#' @return [\code{character(1)}].
#' @export
#' @family filter
listFilterMethods = function() {
  c(
    "linear.correlation",
    "rank.correlation",
    "information.gain",
    "gain.ratio",
    "symmetrical.uncertainty",
    "chi.squared",
    "random.forest.importance",
    "relief",
    "oneR",
    "mRMR.classic"
  )
}

