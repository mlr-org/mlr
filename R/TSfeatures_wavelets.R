#' @title Discrete Wavelet transform features
#'
#' @description The function extracts discrete wavelet transform coefficients
#'   for the raw time series curve data.
#'
#' @param curves [\code{data.frame},\code{matrix}]\cr Time series curve data.
#' @param filter, periodic \cr Optional. Which filter should be used. Default:
#'   \code{filter} = \dQuote{la8}, \code{boundary} = \dQuote{periodic}. See
#'   package \code{wavelets} for more information.
#' @return Returns an \code{data.frame} object containing the wavelet
#'   coefficients.
#'
#' @export
getTSWaveletFeatures = function(curves, filter = NULL, boundary = NULL) {
  requirePackages("wavelets", default.method = "load")

  assert(
    checkClass(curves, "data.frame"),
    checkClass(curves, "matrix")
  )

  if (is.null(filter)) filter = "la8"
  if (is.null(boundary)) boundary = "periodic"

  # if matrix, transform to data.frame so that following code executes correctly
  if(inherits(curves, "matrix"))
    curves = as.data.frame(curves)

  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = wavelets::dwt(a, filter = filter, boundary = boundary)
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}
