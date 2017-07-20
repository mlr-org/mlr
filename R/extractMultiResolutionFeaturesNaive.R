#' @title Multiresolution feature extraction.
#'
#' @description
#' The function extract the mean of a small segments of the curve and stack them
#' as features. The segments length are set in a hierachy way so the features
#' cover different resolution levels.
#'
#' @param data [\code{data.frame}]\cr
#'   Data.frame containing matricies with one row per observation of a single functional
#'   or time series and one column per measurement time point. All entries need to be numeric.
#' @param target [\code{character}]\cr
#'   Name of the target variable. Default: \dQuote{NULL}. The variable is only
#'   set to be consistent with the API.
#' @param cols [\code{character} | \code{numeric}]\cr
#'   Column names or indices, the extraction should be performed on.
#' @param res.level [\code{integer}]\cr
#'   The number of resolution hierachy, each length is divided by a factor of 2.
#' @param shift [\code{numeric}]\cr
#'   The overlapping proportion when slide the window for one step.
#' @return Returns a [\code{matrix}] object with each row containing the
#'   multi-resolution features.
#' @export
extractMultiResFeatures = function(data, target, cols, res.level = 3L, shift = 0.5, curve.lens = NULL) {

  data = data[, cols, drop = FALSE]
  if (is.data.frame(data))
    data = as.matrix(data)
  assertMatrix(data, mode = "numeric")

  # Either call the simple Version getUniFDAMultiResFeatures() or the more
  # complicated getFDAMultiResFeatures().
  # the difference is that for the getFDAMultiResFeatures, the curve is again subdivided into
  # subcurves from which the features are extracted
  if (is.null(curve.lens)) {
    getUniFDAMultiResFeatures(data = data, res.level = res.level, shift = shift)
  } else {
    getFDAMultiResFeatures(data = data, res.level = res.level, shift = shift, curve.lens = curve.lens)
  }
}


#' @title Multiresolution feature extraction on one functional covariate.
#'
#' @description
#' The function extracts the mean of small segments of the curve and returns them
#' as non-functional features. The segments length are set in a hierachy, so the
#' features cover different resolution levels.
#'
#' @param data [\code{data.frame}]\cr
#'   Numeric input matrix containing a single functional covariate.
#' @param res.level [\code{integer}]\cr
#'   The number of resolution hierachy, each length is divided by a factor of 2.
#' @param shift [\code{numeric}]\cr
#'   The overlapping proportion when slide the window for one step.
#' @return Returns a [\code{matrix}] object with each row containing the
#'   multi-resolution features.
getUniFDAMultiResFeatures = function(data, res.level, shift) {
  feat.list = apply(data, 1, getCurveFeatures, res.level = res.level, shift = shift)
  data.frame(t(feat.list))
}

#' @title Multiresolution feature extraction.
#'
#' @description
#' The function extract the mean of a small segments of the curve and stacks them
#' as features. The segments length are set in a hierachy way so the features
#' cover different resolution levels.
#'
#' @param data [\code{dataframe}]\cr
#'   The input matrix.
#' @param curve.lens [\code{vector}]\cr
#'   The subcurve length vector, suggest to sum up to the length of the curve.
#' @param res.level [\code{integer}]\cr
#'   The number of resolution hierachy, each length is divided by a factor of 2.
#' @param shift [\code{numeric}]\cr
#'   The overlapping proportion when slide the window for one step.
#' @return Returns a [\code{matrix}] object with each row containing the
#'   multi-resolution features.
getFDAMultiResFeatures = function(data, res.level = 3L, shift = 0.5, curve.lens) {
  # Assert that curve.lens sums up to ncol(data)
  stopifnot(sum(curve.lens) == ncol(data))

  clsum = cumsum(curve.lens)
  feat.list = apply(data, 1, function(x) {
    # Extract the data from the different subcurves specified by curve.lens
    subfeats = Map(function(seqstart, seqend) {
      getCurveFeatures(x[seqstart:seqend], res.level = res.level, shift = shift)
    }, clsum - curve.lens + 1, cumsum(curve.lens))
    # And return as vector
    unlist(subfeats)
  })
  data.frame(t(feat.list))
}


# @param x [\code{numeric(n)}]\cr
# The input curve
getSegmentFeatures = function(x) {
  mean(x)
}

#'  Get Features from a single (sub-)curve
#'
#' @param x[\code{numeric(n)}]\cr
#' The input curve
#' @param res.level [\code{integer}]\cr
#' The number of hierachy of resolutions
#' @param shift [\code{numeric}]\cr
#' The overlapping proportion when slide the window for one step
#' subroutine for extractFDAMultiResFeatures
getCurveFeatures = function(x, res.level = 3, shift = 0.5) {
  m = length(x)
  start = 1L
  feats = numeric(0L)
  ssize = m  # initialize segment size to be the length of the curve
  for (rl in 1:res.level) {  # ssize is divided by 2 at the end of the loop
    soffset = ceiling(shift * ssize)  # overlap distance
    # messagef("reslev = %i, ssize = %i, soffset=%i", rl, ssize, soffset)
    sstart = 1L
    send = sstart + ssize - 1L  # end position
    while (send <= m) {  # until the segment reach the end
      # messagef("start, end: %i, %i", sstart, send)
      f = getSegmentFeatures(x[sstart:send])
      # print(f)
      feats = c(feats, f)  # append the feats from the last resolution hierachy
      sstart = sstart + soffset
      send = send + soffset
    }
    ssize = ceiling(ssize / 2)  # decrease the segment size
    if (ssize < 1L)  # if the the divide by 2 is too much
      break
  }
  return(feats)
}


# FIXME: The following code block will make all code above obsolete,
# as it uses an equivalent Cpp implementation.
# FIXME: I have commented out this block so as to pass the rcheck()
# command, will uncomment once the bug is fixed
# extractMultiResFeatures2 = function(data, curve.lens, res.level = 3, shift = 0.5) {
#   resmat = matrix(NA_real_, nrow = 1L, ncol = 100000L)
#   .Call(c_get_multires_curve_features, data[1L,,drop = FALSE], curve.lens, resmat, res.level, shift)
#   p = which.first(is.na(resmat[1L,])) - 1L
#   resmat = matrix(0, nrow = nrow(data), ncol = p)
#   .Call(c_get_multires_curve_features, data, curve.lens, resmat, res.level, shift)
#   return(resmat)
# }






