#' @title Get the resampling indices from a nested tuning.
#'
#' @description
#' After you resampled a tuning wrapper (see [makeTuneWrapper])
#' with `resample(..., extract = getTuneResult)` or `resample(..., extract = getFeatSelResult)` this helper returns a `list` with
#' the resampling indices used for the respective method.
#'
#' @param object ([ResampleResult]) \cr
#'   The result of resampling of a tuning wrapper.
#' @param inner ([logical]) \cr
#'   If `TRUE`, returns the inner indices of a nested resampling setting.
#' @return ([list]). One list for each outer resampling fold.
#' @family tune
#' @examples
#' # see example of makeTuneWrapper
#' @export
getResamplingIndices = function(object, inner = FALSE) {
  assertClass(object, "ResampleResult")
  assertList(object$extract)
  if (inner == TRUE) {
    if (!inherits(object$extract[[1]], c("TuneResult", "FeatSelResult"))) {
      stopf("No object of class 'TuneResult' or 'FeatuSelResult' found in slot 'extract'.
             Did you run 'resample()' with 'extract = getTuneResult' or 'extract = getFeatSelResult'?")
    }
    lapply(object$extract, function(x) x$resampling[c("train.inds", "test.inds")])
  } else {
    object$pred$instance[c("train.inds", "test.inds")]
  }
}
