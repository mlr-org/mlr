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
#'   package \code{\link[wavelets]{dwt}} for more information.
#' @return \code{data.frame} object containing the wavelet
#'   coefficients.
#' @export
extractFDAFeatWavelets = function(data, target = NULL, cols, vals = NULL, filter = "la8", boundary = "periodic") {
  requirePackages("wavelets", default.method = "load")
  assertClass(data, "data.frame")
  assertCharacter(filter)
  assertCharacter(boundary)

  df = BBmisc::convertRowsToList(data)
  wtdata = t(BBmisc::dapply(df, fun = function(x) {
    wt = wavelets::dwt(as.numeric(x), filter = filter, boundary = boundary)
    unlist(c(wt@W, wt@V[[wt@level]]))
  }))

  df = as.data.frame(wtdata)
  colnames(df) = stri_paste("wav", filter, boundary, seq_len(ncol(wtdata)), sep = ".")
  df
}
