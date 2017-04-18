#' @title
#' Create a custom feature extraction method for functional data.
#'
#' @description
#' This is a constructor to create your own imputation methods.
#' All built-in methods are listed below.
#' @param learn [\code{function(data, target, cols, ...)}]\cr
#'   Function to learn and extract information on column \code{cols}
#'   out of data frame \code{data}. Argument \code{target} specifies
#'   the target column of the learning task.
#'   The function has to return a named list of values.
#' @param reextract [\code{function(data, target, cols, ...)}]\cr
#'   Function used for reextracting data for predict phase. Can be equal to \code{learn}.
#' @param args [\code{list}]\cr
#'   Named list of arguments to pass to \code{learn} via \code{...}.
#' @family extractFDAFeatures
#' @export
makeExtractFDAFeatMethod = function(learn, reextract, args = list()) {
  assertFunction(learn, args = c("data", "target", "cols"))
  assertFunction(reextract, args = c("data", "target", "cols"))
  assertList(args, names = "named")
  setClasses(list(learn = learn, reextract = reextract, args = args), "extractFDAFeatMethod")
}

#' Built-in imputation methods.
#'
#' The built-ins are:
#' \itemize{
#'   \item \code{extractFDAMedian()} for extracting the median from a curve.
#'   \item \code{extractFDAMean()} for extracting the mean from a curve.
#'   \item \code{extractFDAMinMax()} for extracting the min and max from a curve.
#'   \item \code{extractFDAFourier()} for extracting fourier coefficients from a curve.
#'   \item \code{extractFDAWavelets()} for extracting wavelets from a curve.
#'  }
#' @name extractFDAFeatures
#' @rdname extractFDAFeatures
#' @family extractFDAFeatures
NULL


#' @export
#' @rdname extractFDAFeatures
extractFDAMedian = function() {
  lrn = function(data, target, cols) {data.frame("median" = apply(data[, cols], 1, median, na.rm = TRUE))}
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn
  )
}

#' @export
#' @rdname extractFDAFeatures
extractFDAMean = function() {
  lrn = function(data, target, cols) {data.frame("mean" = apply(data[, cols], 1, mean, na.rm = TRUE))}
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn
  )
}

#' @export
#' @rdname extractFDAFeatures
extractFDAMinMax = function() {
  # Used for quick testing with >1 columns per return
  lrn = function(data, target, cols) {
    data.frame("min" = apply(data[, cols], 1, min, na.rm = TRUE),
      "max" = apply(data[, cols], 1, max, na.rm = TRUE))
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn)
}


#' @export
#' @param trafo.coeff [\code{character}]\cr
#'   See \code{\link[mlr]{extractFourierFeatures}} for more information.
#' @rdname extractFDAFeatures
extractFDAFourier = function(trafo.coeff = "phase") {
  # create a function that calls extractFDAFeatFourier
  assertChoice(trafo.coeff, choices = c("phase", "amplitude"))
  lrn = function(data, target, cols, vals, trafo.coeff) {
    extractFourierFeatures(data = data, target = NULL, cols = cols, trafo.coeff = trafo.coeff)
  }
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn,
    args = list(trafo.coeff = trafo.coeff)
  )
}

#' @export
#' @param filter, boundary [\code{character}]\cr
#'   See \code{\link[mlr]{extractWaveletFeatures}} for more information.
#' @rdname extractFDAFeatures
extractFDAWavelets = function(filter = "la8", boundary = "periodic") {
  lrn = function(data, target, cols, vals, filter, boundary) {
    extractWaveletFeatures(data = data, target = NULL, cols = cols,
      filter = filter, boundary = boundary)
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
    args = list(filter = filter, boundary = boundary)
  )
}

