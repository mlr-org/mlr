#' @title Multiresolution feature extraction.
#'
#' @description
#' The function extracts currently the mean of multiple segments of each curve and stacks them
#' as features. The segments length are set in a hierachy way so the features
#' cover different resolution levels.
#'
#' @param res.level [\code{integer(1)}]\cr
#'   The number of resolution hierachy, each length is divided by a factor of 2.
#' @param shift [\code{numeric(1)}]\cr
#'   The overlapping proportion when slide the window for one step.
#' @param curve.lens [\code{integer}]\cr
#'   Curve subsequence lengths. Needs to sum up to the length of the functional.
#' @return [\code{data.frame}].
#' @export
#' @family fda_featextractor
extractFDAMultiResFeatures = function(res.level = 3L, shift = 0.5, curve.lens = NULL) {

  # Helper function for getFDAMultiResFeatures, extracts for a whole subsequence.
  getUniFDAMultiResFeatures = function(data, res.level, shift) {
    feat.list = apply(data, 1, getCurveFeatures, res.level = res.level, shift = shift)
    data.frame(t(feat.list))
  }

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


  #  Get Features from a single (sub-)curve
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

  getSegmentFeatures = function(x) {
    mean(x)
  }

  lrn = function(data, target, col, res.level, shift, curve.lens) {

    data = data[, col, drop = FALSE]
    if (is.data.frame(data))
      data = as.matrix(data)
    assertMatrix(data, mode = "numeric")

    # The difference is that for the getFDAMultiResFeatures, the curve is again subdivided into
    # subcurves from which the features are extracted
    if (is.null(curve.lens)) {
      getUniFDAMultiResFeatures(data = data, res.level = res.level, shift = shift)
    } else {
      getFDAMultiResFeatures(data = data, res.level = res.level, shift = shift, curve.lens = curve.lens)
    }
  }
  ps = makeParamSet(
    makeIntegerParam("res.level", lower = 1, upper = Inf),
    makeNumericParam("shift", lower = 0, upper = Inf)
  )

  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
    args = list(res.level = res.level, shift = shift, curve.lens = curve.lens), par.set = ps)
}
