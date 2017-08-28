#' @title Discrete Wavelet transform features.
#'
#' @description
#' The function extracts discrete wavelet transform coefficients from the raw
#' functional data.
#'
#' @param data [\code{data.frame},\code{matrix}]\cr
#'   Functional data.
#' @param target [\code{character}]\cr
#'   Name of the target variable.
#' @param include.target [\code{logical}]\cr
#'   Should the target variable (i.e. the label) be added in the returned
#'   data.frame? Default is \code{FALSE}.
#' @param filter,boundary [\code{character}]\cr
#'   Optional, specifies which filter or boundary should be used. Default:
#'   \code{filter} = \dQuote{la8}, \code{boundary} = \dQuote{periodic}. See
#'   package \code{wavelets} for more information.
#' @return Returns an \code{data.frame} object containing the wavelet
#'   coefficients.
#' @export
getFDAWaveletFeatures = function(data, target, include.target = FALSE, filter = NULL, boundary = NULL) {
  requirePackages("wavelets", default.method = "load")

  assert(
    checkClass(data, "data.frame"),
    checkClass(data, "matrix")
  )
  assertCharacter(target)
  assertFlag(include.target)
  assertCharacter(filter, null.ok = TRUE)
  assertCharacter(boundary, null.ok = TRUE)

  if (is.null(filter))
    filter = "la8"
  if (is.null(boundary))
    boundary = "periodic"

  # if matrix, transform to data.frame so that following code executes correctly
  if(inherits(data, "matrix"))
    data = as.data.frame(data)

  cns = colnames(data)
  # potentially extract y-col and remove it from data, we dont need it for fourier-trafo
  if (target %in% cns) {
    y = data[, target]
    data[, target] = NULL
  }

  wtdata = NULL
  for (i in seq_row(data)) {
    a = t(data[i,])
    wt = wavelets::dwt(a, filter = filter, boundary = boundary)
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)

  # potentially include target again
  if (include.target)
    wtdata[, target] = y

  return(list(feat = wtdata, meta = NULL))
}
