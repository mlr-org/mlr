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
      # In some cases the fft values are very small and rounded to 0.
      phase = apply(fft.trafo, 2, function(x) atan(Im(x) / ifelse(abs(Re(x)) < .Machine$double.eps, .Machine$double.eps, Re(x))))
    )

    # If there is only one row in data, fft returns an array
    if (!inherits(fft.pa, "matrix")) {
      fft.pa = as.data.frame(matrix(fft.pa, nrow = 1))
    }
    # Add more legible column names to the output
    df = as.data.frame(fft.pa)
    colnames(df) = stri_paste(trafo.coeff, seq_len(ncol(fft.pa)))
    df
  }

  ps = makeParamSet(makeDiscreteParam("trafo.coeff", values = c("phase", "amplitude")))

  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
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
  assertChoice(filter, filter.vals)
  assertChoice(boundary, c("periodic", "reflection"))

  # FIXME: Add n.levels parameter. This param does not have defaults, I do not know how
  # to handle it right now.
  # @param n.levels [\code{integer(1)}]\cr
  #   Level of decomposition. See \code{\link[wavelets]{dwt}} for details.
  # FIXME: Add n.levels parameter. Has no default. When n.leves are not provided, we do not understand yet if there is a difference

  lrn = function(data, target = NULL, col, filter, boundary) {
    requirePackages("wavelets", default.method = "load")
    assertDataFrame(data)
    assertChoice(col, choices = colnames(data))

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
  ps = makeParamSet(
    makeDiscreteParam("filter", values = filter.vals),
    makeDiscreteParam("boundary", values = c("periodic", "reflection"))
  )
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, args = list(filter = filter, boundary = boundary), par.set = ps)
}



#' @title Time-Series Feature Heuristics
#'
#' @description
#' The function extracts features from functional data based on known Heuristics.
#' For more details refer to \code{\link[tsfeatures]{tsfeatures}}.
#' Under the hood this function uses the package \code{\link[tsfeatures]{tsfeatures}}
#' For more information see Hyndman, Wang and Laptev, Large-Scale Unusual Time Series Detection, ICDM 2015.
#'
#' @param tsfeatures.scale (`logical(1)`)\cr
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
extractFDATsfeatures = function(tsfeatures.scale = TRUE, trim = FALSE, trim_amount = 0.1, parallel = FALSE, na.action = na.pass, ...) {
  lrn = function(data, target = NULL, col, tsfeatures.scale = TRUE, trim = FALSE, trim_amount = 0.1, parallel = FALSE, na.action = na.pass, ...) {
    assertClass(data, "data.frame")
    assertLogical(tsfeatures.scale)
    assertLogical(trim)
    assertLogical(parallel)
    assertNumeric(trim_amount)
    assertFunction(na.action)

    # Transform data to matrix
    data = as.matrix(data[, col, drop = FALSE])
    assertNumeric(data)
    # Convert to list of rows
    row.lst = as.list(data.frame(t(data)))

    requirePackages("tsfeatures", why = "time-series feature extraction")
    # We do not compute heterogeneity, hw_parameters
    feats = c("frequency", "stl_features", "entropy", "acf_features", "arch_stat",
      "crossing_points", "flat_spots", "hurst",  "holt_parameters", "lumpiness",
      "max_kl_shift", "max_var_shift", "max_level_shift", "stability", "nonlinearity")

    tsfeats = tsfeatures::tsfeatures(row.lst, features = feats)

    # Get rid of series and type columns
    tsfeats = data.frame(lapply(tsfeats, as.numeric))
    # Get rid of constant features
    const.feats = which(viapply(tsfeats, function(x) length(unique(x))) == 1L)

    return(tsfeats[, - const.feats])
  }
  ps = makeParamSet(
    makeLogicalParam("tsfeatures.scale", default = TRUE),
    makeLogicalParam("trim", default = FALSE),
    makeNumericParam("trim_amount", lower = 0L, upper = 1L, default = 0.1),
    makeLogicalParam("parallel", default = FALSE),
    makeFunctionParam("na.action", default = na.pass)
  )
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(tsfeatures.scale = tsfeatures.scale, trim = trim, trim_amount = trim_amount, parallel = parallel, na.action = na.action, ...),
    par.set = ps
  )
}

