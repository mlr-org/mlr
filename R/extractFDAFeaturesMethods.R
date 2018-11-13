#' @title Constructor for FDA feature extraction methods.
#'
#' @description
#' This can be used to implement custom FDA feature extraction.
#' Takes a \code{learn} and a \code{reextract} function along with some optional
#' parameters to those as argument.
#'
#' @param learn (`function(data, target, col, ...)`)\cr
#'   Function to learn and extract information on functional column `col`.
#'   Arguments are:
#'   \itemize{
#'   \item data [data.frame]\cr
#'     Data.frame containing matricies with one row per observation of a single functional
#'     or time series and one column per measurement time point.
#'     All entries need to be numeric.
#'   \item target (`character(1)`)\cr
#'     Name of the target variable. Default: \dQuote{NULL}.
#'     The variable is only set to be consistent with the API.
#'   \item col (`character(1)` | `numeric(1)`)\cr
#'     column names or indices, the extraction should be performed on.
#'     The function has to return a named list of values.
#'   }
#' @param reextract (`function(data, target, col, ...)`)\cr
#'   Function used for reextracting data in predict phase.
#'   Can be equal to `learn`.
#' @param args ([list])\cr
#'   Named list of arguments to pass to `learn` via `...`.
#' @param par.set ([ParamSet])\cr
#'   Paramset added to the learner if used in conjunction with a `makeExtractFDAFeatsWrapper`.
#'   Can be `NULL`.`
#' @export
#' @family fda
makeExtractFDAFeatMethod = function(learn, reextract, args = list(), par.set = NULL) {
  assertFunction(learn, args = c("data", "target", "col"))
  assertFunction(reextract, args = c("data", "target", "col"))
  assertList(args, names = "named")
  assertClass(par.set, classes = "ParamSet", null.ok = TRUE)
  setClasses(list(learn = learn, reextract = reextract, args = args, par.set = par.set), "extractFDAFeatMethod")
}

# FIXME: Fix the following methods:
# [ ] DTW
# [ ] FFT
# [ ] PCA
# [ ] BSIGNAL
# [ ] TSFEATURES
# [ ] WAVELETS
# [ ] Fourier
# [ ] Multires


#' @title Fast Fourier transform features.
#'
#' @description
#' The function extracts features from functional data based on the fast fourier
#' transform. For more details refer to [stats::fft].
#'
#' @param trafo.coeff (`character(1)`)\cr
#'   Specifies which transformation of the complex frequency domain
#'   representation should be calculated as a feature representation.
#'   Must be one of \dQuote{amplitude} or \dQuote{phase}.
#'   Default is \dQuote{phase}.
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAFourier = function(trafo.coeff = "phase") {

  lrn = function(data, target = NULL, col, trafo.coeff) {
    assertChoice(trafo.coeff, choices = c("amplitude", "phase"))
    return(list(trafo.coeff = trafo.coeff))
  }

  reextract = function(data, target, col, vals, args) {
    data = checkFDCols(data, col)

    # Calculate fourier coefficients (row wise) which are complex numbers
    fft.trafo = t(apply(data, 1, fft))

    # Extract amplitude or phase of fourier coefficients which are real numbers
    fft.pa = switch(vals$trafo.coeff,
      amplitude = sqrt(apply(fft.trafo, 2, function(x) Re(x)^2 + Im(x)^2)),
      # In some cases the fft values are very small and rounded to 0.
      phase = apply(fft.trafo, 2, function(x) atan(Im(x) / ifelse(abs(Re(x)) < .Machine$double.eps, .Machine$double.eps, Re(x))))
    )

    # If there is only one row in data, fft returns an array
    if (!inherits(fft.pa, "matrix")) {
      fft.pa = as.data.frame(matrix(fft.pa, nrow = 1))
    }
    # Add more legible column names to the output
    df = as.data.frame(fft.pa)
    colnames(df) = stri_paste(vals$trafo.coeff, seq_len(ncol(fft.pa)), sep = ".")
    return(df)
  }

  ps = makeParamSet(makeDiscreteParam("trafo.coeff", values = c("phase", "amplitude")))

  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = reextract,
    args = list(trafo.coeff = trafo.coeff),
    par.set = ps
  )
}


#' @title Discrete Wavelet transform features.
#'
#' @description
#' The function extracts discrete wavelet transform coefficients from the raw
#' functional data.
#' See [wavelets::dwt] for more information.
#'
#' @param filter (`character(1)`)\cr
#'   Specifies which filter should be used.
#'   Must be one of `d`|`la`|`bl`|`c` followed by an even
#'   number for the level of the filter.
#'   The level of the filter needs to be smaller or equal then the time-series length.
#'   For more information and acceptable filters see \code{help(wt.filter)}.
#'   Defaults to `la8`.
#' @param boundary (`character(1)`)\cr
#'   Boundary to be used.
#'   \dQuote{periodic} assumes circular time series,
#'   for \dQuote{reflection} the series is extended to twice its length.
#'   Default is \dQuote{periodic}.
#  @param n.levels [\code{integer(1)}]\cr
#    Level of decomposition. See \code{\link[wavelets]{dwt}} for details.
#    FIXME: Find out how to set this.
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAWavelets = function(filter = "la8", boundary = "periodic") {

  # All possible values for the filters
  filter.vals = c(
    paste0("d", c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)),
    paste0("la", c(8, 10, 12, 14, 16, 18, 20)),
    paste0("bl", c(14, 18, 20)),
    paste0("c", c(6, 12, 18, 24, 30)),
    "haar"
  )

  lrn = function(data, target = NULL, col, filter, boundary) {
    assertChoice(filter, filter.vals)
    assertChoice(boundary, c("periodic", "reflection"))

    # Convert to list in order to catch params that do not have defaults (n.levels)
    vals = learnerArgsToControl(list, filter = filter, boundary = boundary)
    return(vals)
  }

  reextract = function(data, target, col, vals, args) {
    requirePackages("wavelets", default.method = "load")
    data = checkFDCols(data, col)

    # Convert to list of rows and extract wavelets from each time-series.
    rowlst = convertRowsToList(data)
    wtdata = t(dapply(rowlst, fun = function(x) {
      vals$X = as.numeric(x)
      wt = do.call(wavelets::dwt, vals)
      # Extract wavelet coefficients W and level scaling coeffictients V
      unlist(c(wt@W, wt@V[[wt@level]]))
    }))

    df = as.data.frame(wtdata)
    colnames(df) = stri_paste("wav", filter, seq_len(ncol(df)), sep = ".")
    return(df)
  }

  ps = makeParamSet(
    makeDiscreteParam("filter", values = filter.vals),
    makeDiscreteParam("boundary", values = c("periodic", "reflection"))
  )

  makeExtractFDAFeatMethod(learn = lrn, reextract = reextract, args = list(filter = filter, boundary = boundary), par.set = ps)
}


#' @title Extract functional principal component analysis features.
#'
#' @description
#' The function extracts the functional principal components from a data.frame
#' containing functional features.
#'
#' @param rank. (`integer(1)`)\cr
#'   Number of principal components to extract.
#'   Default is `NULL`
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAPCA = function(rank. = NULL, center = TRUE, scale. = FALSE) {
  assertCount(rank., null.ok = TRUE)

  lrn = function(data, target, col, vals, ...) {
    # This method only learns the eigenvectors
    lst = learnerArgsToControl(list, ...)
    lst$x = checkFDCols(data, col)
    rst = do.call("prcomp", lst)
    return(rst)
  }

  reextract = function(data, target, col, vals, args) {
    data = checkFDCols(data, col)
    as.data.frame(predict(vals, data))
  }

  ps = makeParamSet(
    makeIntegerParam("rank.", lower = 1, upper = Inf),
    makeLogicalParam("scale."),
    makeLogicalParam("center")
  )
  makeExtractFDAFeatMethod(learn = lrn, reextract = reextract,
   args = list(rank. = rank., center = center, scale. = scale.),
   par.set = ps)
}

#' @title Bspline mlq features
#'
#' @description
#' The function extracts features from functional data based on the Bspline fit.
#' For more details refer to \code{\link[FDboost]{bsignal}}.
#'
#' @param bsignal.knots (`integer(1)`)\cr
#'   The number of knots for bspline.
#' @param bsignal.df (`numeric(1)`)\cr
#'   The effective degree of freedom of penalized bspline.
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDABsignal = function(bsignal.knots = 10L, bsignal.df = 3) {
  lrn = function(data, target, col, ...) {
    assertInteger(bsignal.knots)
    assertNumeric(bsignal.df)
    return(list(bsignal.df = bsignal.df, bsignal.knots = bsignal.knots))
  }

  reextract = function(data, target, col, vals, args) {
    data = checkFDCols(data, col)
    blrn = FDboost::bsignal(x = data, s = seq_len(ncol(data)), knots = vals$bsignal.knots, degree = vals$bsignal.df)
    feats.bsignal = mboost::extract(object = blrn, what = "design")  # get the design matrix of the base learner
    # Add more legible column names to the output
    df = as.data.frame(feats.bsignal)
    colnames(df) = stri_paste("bsig", seq_len(ncol(df)), sep = ".")
    return(df)
  }
  ps = makeParamSet(
    makeIntegerParam("bsignal.knots", lower = 3L, upper = Inf, default = 10L),
    makeNumericParam("bsignal.df", lower = 0.9, upper = Inf, default = 3)
  )
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = reextract,
    args = list(bsignal.knots = bsignal.knots, bsignal.df = bsignal.df),
    par.set = ps
  )
}

#' @title Time-Series Feature Heuristics
#'
#' @description
#' The function extracts features from functional data based on known Heuristics.
#' For more details refer to \code{\link[tsfeatures]{tsfeatures}}.
#' Under the hood this function uses the package \code{\link[tsfeatures]{tsfeatures}}
#' For more information see Hyndman, Wang and Laptev, Large-Scale Unusual Time Series Detection, ICDM 2015.
#'
#' @param scale (`logical(1)`)\cr
#'   If TRUE, time series are scaled to mean 0 and sd 1 before features are computed.
#' @param trim (`logical(1)`)\cr
#'   If TRUE, time series are trimmed by \code{trim_amount} before features are computed.
#'   Values larger than trim_amount in absolute value are set to NA.
#' @param trim_amount (`numeric(1)`)\cr
#'   Default level of trimming if trim==TRUE.
#' @param parallel (`logical(1)`)\cr
#'   If TRUE, multiple cores (or multiple sessions) will be used.
#'   This only speeds things up when there are a large number of time series.
#' @param na.action (`logical(1)`)\cr
#'   A function to handle missing values. Use na.interp to estimate missing values
#'@param ... (any)\cr
#'   Further arguments passed on to the respective tsfeatures functions.
#' @return ([data.frame])
#' @references Hyndman, Wang and Laptev, Large-Scale Unusual Time Series Detection, ICDM 2015.
#' @export
#' @family fda_featextractor
extractFDATsfeatures = function(scale = TRUE, trim = FALSE, trim_amount = 0.1, parallel = FALSE, na.action = na.pass, ...) {
  lrn = function(data, target, col, ...) {
    assertLogical(scale)
    assertLogical(trim)
    assertLogical(parallel)
    assertNumeric(trim_amount)
    assertFunction(na.action)
    lst = learnerArgsToControl("list", ...)
    # Simply pass on parameters
    return(c(list(scale = scale, trim = trim, parallel = parallel, trim_amount = trim_amount,
      na.action = na.action), lst))
  }

  reextract = function(data, target = NULL, col, vals) {
    data = checkFDCols(data, col)
    # Convert to list of rows
    rowlst = convertRowsToList(data)

    requirePackages("tsfeatures", default.method = "attach")
    # We do not compute some features as they are very unstable.
    # Examples: heterogeneity, hw_parameters
    feats = c("frequency", "stl_features", "entropy", "acf_features", "arch_stat",
      "crossing_points", "flat_spots", "hurst",  "holt_parameters", "lumpiness",
      "max_kl_shift", "max_var_shift", "max_level_shift", "stability", "nonlinearity")


    # Bad implementation in tsfeatures  matches scale to multiple arguments
    #   do.call("tsfeatures", c(list(tslist = rowlst, features = feats), vals))

    tsfeats = tsfeatures::tsfeatures(tslist = rowlst, features = feats, scale = vals$scale,
      trim = vals$trim, parallel = vals$parallel, trim_amount = vals$trim_amount, na.action = vals$na.action)


    # Get rid of series and type columns
    tsfeats = data.frame(lapply(tsfeats, as.numeric))
    # Get rid of constant features
    const.feats = which(viapply(tsfeats, function(x) length(unique(x))) == 1L)
    return(tsfeats[, - const.feats])
  }
  ps = makeParamSet(
    makeLogicalParam("scale", default = TRUE),
    makeLogicalParam("trim", default = FALSE),
    makeNumericParam("trim_amount", lower = 0L, upper = 1L, default = 0.1),
    makeLogicalParam("parallel", default = FALSE),
    makeFunctionParam("na.action", default = na.pass)
  )
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = reextract,
    args = list(scale = scale, trim = trim, trim_amount = trim_amount, parallel = parallel, na.action = na.action, ...),
    par.set = ps
  )
}


#' @title DTW kernel features
#'
#' @description
#' The function extracts features from functional data based on the DTW distance with a reference dataframe.
#'
#' @param ref.method (`character(1)`)\cr
#'   How should the reference curves be obtained?
#'   Method `random` draws `n.refs` random reference curves, while `all` uses all curves as references.
#'   In order to use user-provided reference curves, this parameter is set to `fixed`.
#' @param n.refs (`numeric(1)`)\cr
#'   Number of reference curves to be drawn (as a fraction of the number of observations in the training data).
#' @param refs (`matrix`|`integer(n)`)\cr
#'   Integer vector of training set row indices or a matrix of reference curves with the same length as
#'   the functionals in the training data. Overwrites `ref.method` and `n.refs`.
#' @param dtwwindow (`numeric(1)`)\cr
#'   Size of the warping window size (as a proportion of query length).
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDADTWKernel = function(ref.method = "random", n.refs = 0.05, refs = NULL, dtwwindow = 0.05) {
  requirePackages("rucrdtw", default.method = "attach")

  # Function that extracts dtw-distances for a single observation and a set of reference
  # curves
  getDtwDist = function(frow, refs, dtwwindow) {
    # Compute dtw distance from the selected row to each reference row
    row = vnapply(seq_len(nrow(refs)), function(i) rucrdtw::ucrdtw_vv(frow, refs[i, ], dtwwindow)$distance)
    return(row)
  }

  lrn = function(data, target = NULL, col, ref.method = "random", n.refs = 0.05, refs = NULL, dtwwindow = 0.05) {
    assertChoice(ref.method, c("random", "all", "fixed"))
    assertNumeric(n.refs, lower = 0, upper = 1)
    assertChoice(class(refs), c("matrix", "integer", "NULL"))
    assertNumber(dtwwindow)

    data = checkFDCols(data, col)

    # Obtain reference curves
    if (is.null(refs) | is.integer(refs)) {
      if (ref.method == "random")
        refs = sample(seq_len(nrow(data)), size = max(min(nrow(data), round(n.refs * nrow(data), 0)), 2L))
      if (ref.method == "all")
        refs = seq_len(nrow(data))
      refs.data = data[refs, , drop = FALSE]
    } else {
      assert_true(nrow(refs) == nrow(data))
      refs.data = refs
    }
    # This method only stores and returns the data points we compare against
    return(list(refs = refs.data, dtwwindow = dtwwindow))
  }

  reextract = function(data, target = NULL, col, vals, args) {
    data = checkFDCols(data, col)

    feats.dtw = t(apply(data, 1L, function(x) getDtwDist(x, vals$refs, vals$dtwwindow)))

    # Add more legible column names to the output
    df = as.data.frame(feats.dtw)
    colnames(df) = stri_paste("dtw", seq_len(ncol(df)), sep = ".")
    return(df)
  }

  ps = makeParamSet(
    makeDiscreteParam(id = "ref.method", default = "random", values = c("random", "all", "fixed")),
    makeNumericParam(id = "n.refs", default = 0.05, lower = 0, upper = 1),
    makeUntypedParam(id = "refs", default = NULL),
    makeNumericParam(id = "dtwwindow", lower = 0, upper = 1)
  )

  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = reextract,
    args = list(ref.method = ref.method, n.refs = n.refs, refs = refs, dtwwindow = dtwwindow),
    par.set = ps
  )
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
    for (rl in seq_len(res.level)) {
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
    assertCount(res.level)
    assertNumber(shift)
    assertNumeric(seg.lens, null.ok  = TRUE)
    list(res.level = res.level, shift = shift, seg.lens = seg.lens)
  }


  reextract = function(data, target = NULL, col, vals, args) {
    data = checkFDCols(data, col)

    # The difference is that for the getFDAMultiResFeatures, the curve is again subdivided into
    # subcurves from which the features are extracted
    if (is.null(vals$seg.lens)) {
      df = getCurveFeaturesDF(data = data, res.level = vals$res.level, shift = vals$shift)
    } else {
      df = getFDAMultiResFeatures(data = data, res.level = vals$res.level, shift = vals$shift, seg.lens = vals$seg.lens)
    }

    # For res.level=1 make sure we return the correct dimensions
    if (is.null(dim(df)) | vals$res.level == 1L)
      df = data.frame(t(df))

    rownames(df) = NULL
    colnames(df) = stri_paste("multires", seq_len(ncol(df)), sep = ".")

    return(df)
  }

  ps = makeParamSet(
    makeIntegerParam("res.level", lower = 1L, upper = Inf),
    makeNumericParam("shift", lower = 0.001, upper = 1.0)
  )

  makeExtractFDAFeatMethod(learn = lrn, reextract = reextract,
    args = list(res.level = res.level, shift = shift, seg.lens = seg.lens),
    par.set = ps)
}
