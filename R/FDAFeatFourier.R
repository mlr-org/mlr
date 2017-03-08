#' @title Fast Fourier transform features.
#'
#' @description
#' The function extracts functional data features based on the fast fourier
#' transform.
#'
#' @param data [\code{data.frame}]\cr
#'   Functional data.
#' @param target [\code{character}]\cr
#'   Name of the target variable. Default: \dQuote{NULL}. The variable is only
#'   set to be consistent with the API.
#' @param trafo.coeff [\code{character}]\cr
#'   Specifies which transformation of the complex frequency domain
#'   representation should be calculated as feature representation. Must be one
#'   of \dQuote{amplitude} or \dQuote{phase}. Default: \dQuote{amplitude}.
#' @return Returns an \code{data.frame} object containing the fourier
#'   coefficients.
#' @export
extractFDAFeatFourier = function(data, target = NULL, trafo.coeff = "phase") {
  assertClass(data, "data.frame")
  assertChoice(trafo.coeff, choices = c("amplitude", "phase"))
  # Transform data to matrix for stats::fft
  data = as.matrix(data)
  
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
  as.data.frame(fft.pa)
}
