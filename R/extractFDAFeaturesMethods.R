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
#'   Function used for reextracting data in predict phase. Can be equal to \code{learn}.
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
#'   \item \code{extractFDAFourier(trafo.coeff)} for extracting fourier coefficients from a curve.
#'   \item \code{extractFDAWavelets(filter, boundary)} for extracting wavelets from a curve.
#'  }
#' @name extractFDAFeatMethods
#' @rdname extractFDAFeatMethods
#' @family extractFDAFeatures
NULL


#' @export
#' @rdname extractFDAFeatMethods
extractFDAMedian = function() {
  lrn = function(data, target, cols) {data.frame("median" = apply(data[, cols, drop = FALSE], 1, median, na.rm = TRUE))}
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn
  )
}

#' @export
#' @rdname extractFDAFeatMethods
extractFDAMean = function() {
  lrn = function(data, target, cols) {data.frame("mean" = apply(data[, cols, drop = FALSE],
    1, mean, na.rm = TRUE))}
  makeExtractFDAFeatMethod(
    learn = lrn,
    reextract = lrn
  )
}

#' @export
#' @rdname extractFDAFeatMethods
extractFDAMinMax = function() {
  # Used for quick testing with >1 columns per return
  lrn = function(data, target, cols) {
    data.frame("min" = apply(data[, cols, drop = FALSE], 1, min, na.rm = TRUE),
      "max" = apply(data[, cols], 1, max, na.rm = TRUE))
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn)
}


#' @export
#' @inheritParams extractFourierFeatures
#' @rdname extractFDAFeatMethods
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
#' @inheritParams extractWaveletFeatures
#' @rdname extractFDAFeatMethods
extractFDAWavelets = function(filter = "la8", boundary = "periodic") {
  lrn = function(data, target, cols, vals, filter, boundary) {
    extractWaveletFeatures(data = data, target = NULL, cols = cols,
      filter = filter, boundary = boundary)
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
    args = list(filter = filter, boundary = boundary)
  )
}

#' @export
#' @inheritParams extractFpcaFeatures
#' @rdname extractFDAFeatMethods
extractFDAFpca = function(pve = 0.99, npc = NULL) {
  lrn = function(data, target, cols, vals, pve, npc) {
    extractFpcaFeatures(data = data, target = NULL, cols = cols, pve = pve, npc = npc)
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, args = list(pve = pve, npc = npc))
}

#' @export
#' @inheritParams extractMultiResFeatures
#' @rdname extractFDAFeatMethods
extractFDAMultiResFeatures = function(res.level = 3L, shift = 0.5, curve.lens = NULL) {
  lrn = function(data, target, cols, vals, res.level = 3L, shift = 0.5, curve.lens = NULL) {
    extractMultiResFeatures(data, target, cols, res.level, shift, curve.lens)
  }
  makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
    args = list(res.level = res.level, shift = shift, curve.lens = curve.lens))
}
