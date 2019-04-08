#' @title Constructor for FDA feature extraction methods.
#'
#' @description
#' This can be used to implement custom feature FDA extraction.
#'
#' @param learn (`function(data, target, col, ...)`)\cr
#'   Function to learn and extract information on functional column `col`.
#'   Arguments are:
#'   \itemize{
#'   \item data [data.frame]\cr
#'     Data.frame with one row per observation of a single functional feature
#'     or time series and one column per measurement time point.
#'     All entries need to be numeric.
#'   \item data [data.frame]\cr
#'     Data.frame containing matricies with one row per observation of a single functional
#'     or time series and one column per measurement time point. All entries need to be numeric.
#'   \item target [character]\cr
#'     Name of the target variable. Default: \dQuote{NULL}.
#'     The variable is only set to be consistent with the API.
#'   \item col ([character] | [numeric])\cr
#'     column names or indices, the extraction should be performed on.
#'     The function has to return a named list of values.
#'   }
#' @param reextract (`function(data, target, col, ...)`)\cr
#'   Function used for reextracting data in predict phase.
#'   Can be equal to `learn`.
#' @param args ([list])\cr
#'   Named list of arguments to pass to `learn` via `...`.
#' @export
#' @family fda
makeExtractFDAFeatMethod = function(learn, reextract, args = list()) {
  assertFunction(learn, args = c("data", "target", "col"))
  assertFunction(reextract, args = c("data", "target", "col"))
  assertList(args, names = "named")
  setClasses(list(learn = learn, reextract = reextract, args = args), "extractFDAFeatMethod")
}

#' @title Fast Fourier transform features.
#'
#' @description
#' The function extracts features from functional data based on the fast fourier
#' transform. For more details refer to [stats::fft].
#'
#' @param trafo.coeff ([character])\cr
#'   Specifies which transformation of the complex frequency domain
#'   representation should be calculated as a feature representation.
#'   Must be one of \dQuote{amplitude} or \dQuote{phase}.
#'   Default is \dQuote{phase}.
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAFourier = function(trafo.coeff = "phase") {
  # create a function that calls extractFDAFeatFourier
  assertChoice(trafo.coeff, choices = c("phase", "amplitude"))

  lrn = function(data, target = NULL, col, trafo.coeff) {

    assertClass(data, "data.frame")
    assertChoice(trafo.coeff, choices = c("amplitude", "phase"))

    # Transform data to matrix for stats::fft
    data = as.matrix(data[, col, drop = FALSE])
    assertNumeric(data)

    # Calculate fourier coefficients (row wise) which are complex numbers
    fft.trafo = t(apply(data, 1, fft))
    # Extract amplitude or phase of fourier coefficients which are real numbers
    fft.pa = switch(trafo.coeff,
      amplitude = sqrt(apply(fft.trafo, 2, function(x) Re(x)^2 + Im(x)^2)),
      phase = apply(fft.trafo, 2, function(x) atan(Im(x) / Re(x)))
    )

    # If there is only one row in data, fft returns an array
    if (!inherits(fft.pa, "matrix")) {
      fft.pa = as.data.frame(matrix(fft.pa, nrow = 1))
    }
    # Add more legible column names to the output
    df = as.data.frame(fft.pa)
    colnames(df) = stri_paste(trafo.coeff, seq_len(ncol(fft.pa)), sep = ".")
    df
  }
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(trafo.coeff = trafo.coeff)
  )
}


#' @title Discrete Wavelet transform features.
#'
#' @description
#' The function extracts discrete wavelet transform coefficients from the raw
#' functional data.
#' See [wavelets::dwt] for more information.
#'
#' @param filter ([character])\cr
#'   Specifies which filter should be used.
#'   Default is \dQuote{la8}.
#' @param boundary ([character])\cr
#'   Boundary to be used.
#'   \dQuote{periodic} assumes circular time series,
#'   for \dQuote{reflection} the series is extended to twice its length.
#'   Default is \dQuote{periodic}.
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAWavelets = function(filter = "la8", boundary = "periodic") {
  assertCharacter(filter)
  assertChoice(boundary, c("periodic", "reflection"))

  lrn = function(data, target = NULL, col, filter, boundary) {
    requirePackages("wavelets", default.method = "load")

    assertClass(data, "data.frame")
    assertNumeric(as.matrix(data[, col]))

    df = convertRowsToList(data[, col, drop = FALSE])
    wtdata = t(dapply(df, fun = function(x) {
      wt = wavelets::dwt(as.numeric(x), filter = filter, boundary = boundary)
      unlist(c(wt@W, wt@V[[wt@level]]))
    }))

    df = as.data.frame(wtdata)
    colnames(df) = stri_paste("wav", filter, seq_len(ncol(wtdata)), sep = ".")
    return(df)
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, args = list(filter = filter, boundary = boundary))
}

#' @title Extract functional principal component analysis features.
#'
#' @description
#' The function extracts the functional principal components from a data.frame
#' containing functional features.
#'
#' @param pve ([numeric])\cr
#'   Fraction of variance explained for the functional principal components.
#'   Default is 0.99.
#' @param npc ([integer])\cr
#'   Number of principal components to extract. Overrides `pve` param.
#'   Default is `NULL`
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAFPCA = function(pve = 0.99, npc = NULL) {
  assertNumber(pve, lower = 0, upper = 1)
  assertCount(npc, null.ok = TRUE)

  lrn = function(data, target, col, vals, pve, npc) {
    requirePackages("mboost", default.method = "load")
    requirePackages("refund", default.method = "load")
    assert(
      checkClass(data, "data.frame"),
      checkClass(data, "matrix")
    )

    data = data[, col, drop = FALSE]

    # transform dataframe into matrix
    if (inherits(data, "data.frame"))
      data = as.matrix(data)

    # extract fpca features
    # FIXME: Add other fpca. options, maybe via function args ?
    rst = refund::fpca.sc(Y = data, pve = pve, npc = npc)
    # Order the columns by score
    features.fpca = rst$scores[, order(rst$evalues,  decreasing = TRUE)]
    df.fpca = as.data.frame(features.fpca)
    names(df.fpca) = paste0("Fpca", seq_len(ncol(df.fpca)))
    return(df.fpca)
  }

  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, args = list(pve = pve, npc = npc))
}

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
#' @param curve.lens ([integer])\cr
#'   Curve subsequence lengths. Needs to sum up to the length of the functional.
#' @return ([data.frame]).
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

  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
    args = list(res.level = res.level, shift = shift, curve.lens = curve.lens))
}
