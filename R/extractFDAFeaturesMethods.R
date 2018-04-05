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
#' @param filter [`character(1)`]\cr
#'   Specifies which filter should be used.
#'   Must be one of `d`|`la`}|`bl`|`c` followed by an even
#'   number for the level of the filter.
#'   The level of the filter needs to be smaller or equal then the time-series length.
#'   For more information and acceptable filters see \code{help(wt.filter)}.
#'   Defaults to `la8`.
#' @param boundary [character(1)]\cr
#'   Boundary to be used.
#'   \dQuote{periodic} assumes circular time series,
#'   for \dQuote{reflection} the series is extended to twice its length.
#'   Default is \dQuote{periodic}.
#' @param n.levels [\code{integer(1)}]\cr
#'   Level of decomposition. See \code{\link[wavelets]{dwt}} for details.
#' @return ([data.frame]).
#' @export
#' @family fda_featextractor
extractFDAWavelets = function(filter = "la8", boundary = "periodic") {

  # All possible values for the filters
  filter.vals = c(
    paste0("d", c(2,4,6,8,10,12,14,16,18,20)),
    paste0("la", c(8,10,12,14,16,18,20)),
    paste0("bl", c(14,18,20)),
    paste0("c", c(6,12,18,24,30))
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
  ps = makeParamSet(makeNumericParam("pve", lower = 0, upper = 1))
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, args = list(pve = pve, npc = npc), par.set = ps)
}


#' @title Bspline mlq features
#'
#' @description
#' The function extracts features from functional data based on the Bspline fit.
#' For more details refer to \code{\link[FDboost]{bsignal}}.
#'
#' @param bsignal.knots [\code{integer}]\cr
#' The number of knots for bspline.
#' @param bsignal.df [\code{numeric}]\cr
#' The effective degree of freedom of penalized bspline.
#' @return [\code{data.frame}].
#' @export
#' @family fda_featextractor
extractFDABsignal = function(bsignal.knots = 10L, bsignal.df = 3) {
  lrn = function(data, target = NULL, col, bsignal.knots, bsignal.df) {
    assertClass(data, "data.frame")
    assertInteger(bsignal.knots)
    assertNumeric(bsignal.df)
    # Transform data to matrix for stats::fft
    data = as.matrix(data[, col, drop = FALSE])
    assertNumeric(data)
    blrn = FDboost::bsignal(x = data, s = 1:ncol(data), knots = bsignal.knots, degree = bsignal.df)
    feats_bsignal = mboost::extract(object = blrn, what = "design")  # get the design matrix of the base learner
    # Add more legible column names to the output
    df = as.data.frame(feats_bsignal)
    colnames(df) = stri_paste("bsig", seq_len(ncol(df)), sep = ".")
    df
  }
  ps = makeParamSet(
    makeIntegerParam("bsignal.knots", lower = 1L, upper = Inf, default = 10L),
    makeIntegerParam("bsignal.df", lower = 1L, upper = Inf, default = 3L)
  )
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(bsignal.knots = bsignal.knots, bsignal.df = bsignal.df),
    par.set = ps
  )
}

