#' @title Discrete Wavelet transform features.
#'
#' @description
#' The function extracts discrete wavelet transform coefficients from the raw
#' functional data.
#'
#' @param data [\code{data.frame}]\cr
#'   Data.frame with one row per observation of a single functional or time series and
#'   one column per measurement time point. All entries need to be numeric.
#' @param target [\code{character}]\cr
#'   Name of the target variable. Default: \dQuote{NULL}. The variable is only
#'   set to be consistent with the API.
#' @param cols [\code{character} | \code{numeric}]\cr
#'   Column names or indices, the extraction should be performed on.
#' @param filter [\code{character}]\cr
#'   Specifies which filter should be used.
#'   Default: \code{filter} = \dQuote{la8}.
#'   See \code{\link[wavelets]{dwt}} for more information.
#' @param boundary [\code{character}]\cr
#'   Boundary to be used.
#'   The default, \code{boundary} = \dQuote{periodic} assumes circular time series.
#'   For \code{boundary} = \dQuote{reflection} the series is extended to twice its length.
#'   See \code{\link[wavelets]{dwt}} for more information.
#' @return \code{data.frame} object containing the wavelet coefficients.
#' @export
extractWaveletFeatures = function(data, target = NULL, cols, filter = "la8", boundary = "periodic") {
  requirePackages("wavelets", default.method = "load")

  assertClass(data, "data.frame")
  assertNumeric(as.matrix(data[, cols]))
  assertCharacter(filter)
  assertChoice(boundary, c("periodic", "reflection"))

  df = BBmisc::convertRowsToList(data[, cols])
  wtdata = t(BBmisc::dapply(df, fun = function(x) {
    wt = wavelets::dwt(as.numeric(x), filter = filter, boundary = boundary)
    unlist(c(wt@W, wt@V[[wt@level]]))
  }))

  df = as.data.frame(wtdata)
  colnames(df) = stri_paste("wav", filter, boundary, seq_len(ncol(wtdata)), sep = ".")
  df
}
