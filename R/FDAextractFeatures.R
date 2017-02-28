#' @title Extract functional data features.
#'
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
  assertChoice(method, choices = c("wavelets", "fourier"))
  # if matrix, transform to data.frame so that following code executes correctly
  if(inherits(data, "matrix"))
    data = as.data.frame(data)

  new.args = c(list(data = data, target = target), as.list(unlist(args)))

  switch(method,
    wavelets = {tsf = do.call(getFDAWaveletFeatures, new.args)},
    fourier = {tsf = do.call(getFDAFourierFeatures, new.args)}
  )
  return(tsf)
}
