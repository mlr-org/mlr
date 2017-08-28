#' @title Extract functional data features.
#'
#FIXME: clarify what function does exactly, that it assumes one func covar per row
#FIXME: and document ALL possible extract methods here in a nice list
#' @description
#' The function is the general framework to **extract** feature transformation
#' from raw functional data. This is a kind of preprocessing step.
#'
#' @param data [\code{data.frame},\code{matrix}]\cr
#'   Functional data.
#' @param target [\code{character}]\cr
#'   Name of the target variable.
#' @param method [\code{character}]\cr
#'   Which method is used to extract functional data features. Methods available.
#'   Wavelet transformation: \dQuote{wavelets}.
#'   Fourier transformation: \dQuote{fourier}.
#' @param args \cr
#'   Additional arguments passed to the features functions.
#' @return Returns an \code{data.frame} object containing the transformed data.
#' @export
extractFDAFeatures = function(data, target, method, args) {
  assert(
    checkClass(data, "data.frame"),
    checkClass(data, "matrix")
  )
  assertCharacter(target)
  #FIXME: dont mix capitalization styles so much. "multires" is better.
  assertChoice(method, choices = c("wavelets", "fourier", "bsignal", "multiRes", "fpca"))
  # if matrix, transform to data.frame so that following code executes correctly
  if (is.matrix(data))
    data = as.data.frame(data) # FIXME: This line is not so nice , but could make Fourier work

  args = c(list(data = data, target = target), args)

  fun = switch(method,
    wavelets = getFDAWaveletFeatures,
    fourier = getFDAFourierFeatures,
    bsignal = getFDAFDboostFeatures,
    multiRes = getFDAMultiResFeatures,
    fpca = getFDAFPCAFeatures
  )
  res = do.call(fun, args)
  return(list(feat = res$feat, meta = res$meta))
}

#FIXME: i dont like a) the name of this function b) that we need it
#FIXME: we should get rid of this, but we need it for the PreprocWrapper but that cannot work
#FIXME: on tasks now in the API.

#' @title MultiFDACovariate feature extraction.
#'
#' @description
#' The function extract the features for each functional covariate of an FDA dataframe and bind them to
#' a new dataframe.
#' Currently, the scalar features are not binded to the output, in other words, they are abandoned.
#'
#' @param data [\code{dataframe}]\cr
#'   The input dataframe.
#' @param target [\code{character}]\cr
#'   Name of the target variable.
#' @param fd.features [\code{list}] \cr
#'   The hash table for different functional covariate. See [\code{\link{FDARegrTask}}].
#' @param method [\code{character}]\cr
#'   Which method is used to extract functional data features. Methods available.
#'   Wavelet transformation: \dQuote{wavelets}.
#'   Fourier transformation: \dQuote{fourier}.
#' @param args \cr
#'   Additional arguments passed to the features functions.
#' @return Returns a [\code{matrix}] object with each row containing the
#'   multi-resolution features.
#' @export
extractMultiFDAFeatures = function(data, target, fd.features, method, args) {
  fdns = names(fd.features)
  res_ft = namedList(fdns)
  res_meta = namedList(fdns)
  #FIXME: Currently only support equal parameter to each channel
  for (fdn in fdns) {
    res = extractFDAFeatures(data[, fd.features[[fdn]], drop = FALSE], target, method, args)
    res_ft[[fdn]] = res$feat
    res_meta[[fdn]] = res$meta
  }
  ft = do.call(cbind, res_ft)
  return(list(feat = ft, meta.list = res_meta))
}
