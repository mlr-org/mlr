#' @title Fast Fourier transform features
#'
#' @description The functino creates time series features based on the fast
#'   discrete fourier transform.
#'
#' @param curves \code{data.frame},\code{matrix}]\cr Time series curve data.
#' @param fft.coeff \code{character}\cr Optional. Which 'transformation' of the
#'   complex frequency domain representation should be calculated as feature
#'   representation. Must be one of \dQuote{amplitude} or \dQuote{Phase}.
#'   Default: \dQuote{amplitude}.
#' @return Returns an \code{data.frame} object containing the fourier
#'   coefficients.
#'
#' @export
getTSFourierFeatures = function(curves, fft.coeff = NULL) {
  requirePackages("stats", default.method = "load")

  assert(
    checkClass(curves, "data.frame"),
    checkClass(curves, "matrix")
  )

  if( is.null(fft.coeff) ) fft.coeff = "amplitude"
  if ( !(fft.coeff %in%  c("amplitude", "phase")) )
    stop("Transformation for complex frequency domain must be one of 'amplitude' or 'phase'. Please check method.")

  # if data.frame, transform to matrix so that following code executes correctly
  if(inherits(curves, "data.frame"))
    curves = as.matrix(curves)

  # calculate fourier coefficients -> complex numbers
  ffttrafo = t(apply(curves,1, fft))

  # extract amplitude or phase of fourier coefficients
  switch(fft.coeff,
         amplitude = {fftPA = getFourierAmplitude(fourier.comp = ffttrafo)},
         phase = {fftPA = getFourierPhase(fourier.comp = ffttrafo)}
         )
  print(fft.coeff)

  if(!inherits(fftPA, "matrix"))
    fftPA = as.data.frame(matrix(fftPA, nrow = 1)) else
      fftPA = as.data.frame(fftPA)

  return(fftPA)
}

#' @export
getFourierAmplitude = function(fourier.comp){
  amp = apply(fourier.comp, 2, function(x) Re(x)^2 + Im(x)^2 ) # compute for each row
  amp = sqrt(amp)
  return(amp)
}

#' @export
getFourierPhase = function(fourier.comp){
  pha = apply(fourier.comp, 2, function(x) atan( Im(x) / Re(x) ) )
  return(pha)
}
