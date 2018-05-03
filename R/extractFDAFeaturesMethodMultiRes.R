#' @title Multiresolution feature extraction.
#'
#' @description
#' The function extracts currently the mean of multiple segments of each curve and stacks them
#' as features. The segments length are set in a hierachy way so the features
#' cover different resolution levels.
#'
#' @param res.level (`integer(1)`)\cr
#'   The number of resolution hierachy, each length is divided by a factor of 2.
#' @param shift (`numeric(1)`)\cr
#'   The overlapping proportion when slide the window for one step.
#' @param seg.lens (`integer(1)`)\cr
#'   Curve subsequence lengths. Needs to sum up to the length of the functional.
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAMultiResFeatures = function(res.level = 3L, shift = 0.5, seg.lens = NULL) {

  getCurveFeaturesDF = function(data, res.level = 3L, shift = 0.5) {
    feat.list = apply(data, 1, getCurveFeatures, res.level = res.level, shift = shift)
    df = data.frame(t(feat.list))
    return(df)
  }

  getFDAMultiResFeatures = function(data, res.level = 3L, shift = 0.5, seg.lens = NULL) {
    # Assert that seg.lens sums up to ncol(data)
    stopifnot(sum(seg.lens) == ncol(data))

    clsum = cumsum(seg.lens)
    feat.list = apply(data, 1, function(x) {
      # Extract the data from the different subcurves specified by seg.lens
      # the start of the seg is clsum - seg.lens + 1, the end of the seg is cumsum(seg.lens)
      # ex: seg.lens = c(2, 3, 4), clsum = c(2, 5, 9), clsum - seg.lens +1 = 1, 3, 6
      subfeats = Map(function(seqstart, seqend) {
        getCurveFeatures(x[seqstart:seqend], res.level = res.level, shift = shift)
      }, clsum - seg.lens + 1, cumsum(seg.lens))
      # And return as vector
      unlist(subfeats)
    })
    df = data.frame(t(feat.list))
    return(df)
  }


  #  Get Features from a single (sub-)curve
  getCurveFeatures = function(x, res.level = 3L, shift = 0.5) {
    m = length(x)
    feats = numeric(0L)
    ssize = m  # initialize segment size to be the length of the curve
    for (rl in 1:res.level) {
      # ssize is divided by 2 at the end of the loop
      soffset = ceiling(shift * ssize)  # overlap distance
      # messagef("reslev = %i, ssize = %i, soffset=%i", rl, ssize, soffset)
      sstart = 1L
      send = sstart + ssize - 1L  # end position
      while (send <= m) {
        # until the segment reach the end
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

  lrn = function(data, target, col, res.level = 3L, shift = 0.5, seg.lens = NULL) {

    data = data[, col, drop = FALSE]
    if (is.data.frame(data))
      data = as.matrix(data)
    assertMatrix(data, mode = "numeric")

    # The difference is that for the getFDAMultiResFeatures, the curve is again subdivided into
    # subcurves from which the features are extracted
    if (is.null(seg.lens)) {
      df = getCurveFeaturesDF(data = data, res.level = res.level, shift = shift)
    } else {
      df = getFDAMultiResFeatures(data = data, res.level = res.level, shift = shift, seg.lens = seg.lens)
    }

    # For res.level=1 make sure we return the correct dimensions
    if(res.level == 1L)
      df = data.frame(t(df))

    rownames(df) = NULL
    colnames(df) = stri_paste("multires", seq_len(ncol(df)), sep = ".")
    return(df)
  }
  ps = makeParamSet(
    makeIntegerParam("res.level", lower = 1L, upper = 5L),
    makeNumericParam("shift", lower = 0.001, upper = 1.0)
  )

  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
    args = list(res.level = res.level, shift = shift, seg.lens = seg.lens), par.set = ps)
}
