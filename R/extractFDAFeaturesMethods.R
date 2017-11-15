#' @title Constructor for FDA feature extraction methods.
#'
#' @description
#' This can be used to implement custom FDA feature extraction.
#' Takes a \code{learn} and a \code{reextract} function along with some optional
#' parameters to those as argument.
#'
#' @param learn [\code{function(data, target, col, ...)}]\cr
#'   Function to learn and extract information on functional column \code{col}.
#'   Arguments are:
#'   \itemize{
#'   \item data [\code{data.frame}]\cr
#'     Data.frame containing matricies with one row per observation of a single functional
#'     or time series and one column per measurement time point.
#'     All entries need to be numeric.
#'   \item target [\code{character}]\cr
#'     Name of the target variable. Default: \dQuote{NULL}.
#'     The variable is only set to be consistent with the API.
#'   \item col [\code{character} | \code{numeric}]\cr
#'     column names or indices, the extraction should be performed on.
#'     The function has to return a named list of values.
#'   }
#' @param reextract [\code{function(data, target, col, ...)}]\cr
#'   Function used for reextracting data in predict phase.
#'   Can be equal to \code{learn}.
#' @param args [\code{list}]\cr
#'   Named list of arguments to pass to \code{learn} via \code{...}.
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
#' transform. For more details refer to \code{\link[stats]{fft}}.
#'
#' @param trafo.coeff [\code{character}]\cr
#'   Specifies which transformation of the complex frequency domain
#'   representation should be calculated as a feature representation.
#'   Must be one of \dQuote{amplitude} or \dQuote{phase}.
#'   Default is \dQuote{phase}.
#' @return [\code{data.frame}].
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
#' See \code{\link[wavelets]{dwt}} for more information.
#'
#' @param filter [\code{character(1)}]\cr
#'   Specifies which filter should be used. Default is \dQuote{la8}.
#'   Must be one of \code{d}|\code{la}|\code{bl}|\code{c} followed by an even
#'   number for the level of the filter.
#'   For more information and acceptable filters see \code{help(wt.filter)}.
#' @param boundary [\code{character(1)}]\cr
#'   Boundary to be used.
#'   \dQuote{periodic} assumes circular time series,
#'   for \dQuote{reflection} the series is extended to twice its length.
#'   Default is \dQuote{periodic}.
#' @param n.levels [\code{integer(1)}]\cr
#'   Level of decomposition. See \code{\link[wavelets]{dwt}} for details.
#' @return [\code{data.frame}].
#' @export
#' @family fda_featextractor
extractFDAWavelets = function(filter = "la8", boundary = "periodic") {
  assertString(filter, pattern = "((d|la|bl|c)\\d*[02468])|haar")
  assertChoice(boundary, c("periodic", "reflection"))
  # FIXME: Add n.levels parameter. Has no default, do not know how to handle it right now.
  lrn = function(data, target = NULL, col, filter, boundary) {
    requirePackages("wavelets", default.method = "load")

    assertDataFrame(data)
    assertChoice(col, choices = colnames(data))
    # Convert to list in order to catch params that do not have defaults (n.levels)
    args = learnerArgsToControl(list, filter = filter, boundary = boundary)

    # Convert to list of rows and extract wavelets from each time-series.
    rowlst = convertRowsToList(data[, col, drop = FALSE])
    wtdata = t(dapply(rowlst, fun = function(x) {
      args$X = as.numeric(x)
      wt = do.call(wavelets::dwt, args)
      # Extract wavelet coefficients W and level scaling coeffictients V
      unlist(c(wt@W, wt@V[[wt@level]]))
    }))
    df = as.data.frame(wtdata)
    colnames(df) = stri_paste("wav", filter, seq_len(ncol(df)), sep = ".")
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
#' @param pve [\code{numeric}]\cr
#'   Fraction of variance explained for the functional principal components.
#'   Default is 0.99.
#' @param npc [\code{integer}]\cr
#'   Number of principal components to extract. Overrides \code{pve} param.
#'   Default is \code{NULL}
#' @return [\code{data.frame}].
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
#' The function extracts the mean of multiple segments of each curve
#' as non-functional features. This is done by sequentially dividing the
#' functional feature up into smaller sub-curves of length \code{l/2},
#' where \code{l} is the length of the curve in the previous iteration.
#' In each iteration, a sliding window of length \code{sub_curve_length} is shifted by
#' \code{sub_curve_length} times \code{shift} data points.
#' The means of each sliding window are the new features.
#' Because the extraction happens in a hierarchical manner
#' (that is, in each iteration smaller segments are considered), the features
#' cover different resolution levels of the curve.
#'
#' @param res.level [\code{integer(1)}]\cr
#'   The resolution depth, each length is divided by a factor of 2.
#' @param shift [\code{numeric(1)}]\cr
#'   The overlapping proportion when slide the window for one step.
#' @param curve.lens [\code{integer}]\cr
#'   Instead of splitting the curves in half from the top, a vector of sub-curves can be
#'   can be specified. Specifies the curve subsequence's lengths.
#'   Needs to sum up to the length of the functional.
#' @return [\code{data.frame}].
#' @export
#' @family fda_featextractor
extractFDAMultiResFeatures = function(res.level = 3L, shift = 0.5, curve.lens = NULL) {
  res.level = asCount(res.level)
  assertNumber(shift, lower = 0L, upper = 1L)
  assertInteger(curve.lens, null.ok = TRUE)

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

    assertChoice(col, choices = colnames(data))
    data = as.matrix(data[, col, drop = FALSE])
    assertMatrix(data, mode = "numeric")

    # The difference is that for the getFDAMultiResFeatures, the curve is again subdivided into
    # subcurves from which the features are extracted.
    if (is.null(curve.lens)) {
      df = getUniFDAMultiResFeatures(data = data, res.level = res.level, shift = shift)
    } else {
      df = getFDAMultiResFeatures(data = data, res.level = res.level, shift = shift, curve.lens = curve.lens)
    }
    names(df) = paste0("multires", seq_len(ncol(df)))
    return(df)
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
    args = list(res.level = res.level, shift = shift, curve.lens = curve.lens))
}
