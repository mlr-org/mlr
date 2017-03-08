#' @title Discrete Wavelet transform features.
#'
#' @description
#' The function extracts discrete wavelet transform coefficients from the raw
#' functional data.
#'
#' @param data [\code{data.frame}]\cr
#'   Functional data.
#' @param target [\code{character}]\cr
#'   Name of the target variable. Default: \dQuote{NULL}. The variable is only
#'   set to be consistent with the API.
#' @param filter,boundary [\code{character}]\cr
#'   Specifies which filter or boundary should be used. Default:
#'   \code{filter} = \dQuote{la8}, \code{boundary} = \dQuote{periodic}. See
#'   package \code{wavelets} for more information.
#' @return Returns an \code{data.frame} object containing the wavelet
#'   coefficients.
#' @export
extractFDAFeatWavelets = function(data, target = NULL, filter = "la8", boundary = "periodic") {
  requirePackages("wavelets", default.method = "load")
  assertClass(data, "data.frame")
  assertCharacter(filter)
  assertCharacter(boundary)

  # Create wavelet coeffs row wise
  wtdata = NULL
  for (i in seq_row(data)) {
    a = t(data[i,])
    wt = wavelets::dwt(a, filter = filter, boundary = boundary)
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  as.data.frame(wtdata)
}
