#' @title Get the resampling indices from a nested tuning.
#'
#' @description
#' After you resampled a tuning wrapper (see [makeTuneWrapper])
#' with `resample(..., extract = getTuneResult)` this helper returns a `list` with
#' the resampling indices used for tuning..
#'
#' @param object ([ResampleResult]) \cr
#'   The result of resampling of a tuning wrapper.
#' @return ([list]). One list for each outer resampling fold.
#' @family tune
#' @examples
#' # see example of makeTuneWrapper
#' @export
getNestedResamplingIndices = function(object) {
  assertClass(object, "ResampleResult")
  assertList(object$extract)
  lapply(object$extract, function(x) x$resampling[c("train.inds", "test.inds")])
}
