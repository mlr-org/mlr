#' Get predictions from resample results.
#'
#' @param res [\code{ResampleResult}]\cr
#'   The result of \code{\link{resample}} run with \code{keep.pred = TRUE}.
#' @return [\code{ResamplePrediction}]
##' @export
#' @family resample
getRRPredictions = function(res) {
  if (is.null(res$pred))
    stopf("The prediction slot is empty because the ResampleResult was generated with keep.pred = FALSE.")
  else
    res$pred
}