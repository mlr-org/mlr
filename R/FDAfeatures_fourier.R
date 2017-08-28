#' @title Fast Fourier transform features.
#'
#' @description
#' The function creates functional data features based on the fast discrete
#' fourier transform.
#'
#' @param data [\code{data.frame},\code{matrix}]\cr
#'   Functional data.
#' @param target [\code{character}]\cr
#'   Name of the target variable.
#' @param include.target [\code{logical}]\cr
#'   Should the target variable (i.e. the label) be added in the returned
#'   data.frame? Default is \code{FALSE}.
#' @param fft.coeff [\code{character}]\cr
#'   Optional, specifies which 'transformation' of the complex frequency domain
#'   representation should be calculated as feature representation. Must be one
#'   of \dQuote{amplitude} or \dQuote{phase}. Default: \dQuote{amplitude}.
#' @return Returns an \code{data.frame} object containing the fourier
#'   coefficients.
#' @export
getFDAFourierFeatures = function(data, target, include.target = FALSE, fft.coeff = "phase") {
  requirePackages("stats", default.method = "load")

  assert(
    checkClass(data, "data.frame"),
    checkClass(data, "matrix")
  )
  assertCharacter(target)
  assertFlag(include.target)
  assertChoice(fft.coeff, choices = c("amplitude", "phase"))

  # potentially extract y-col and remove it from data, we dont need it for fourier-trafo
  cns = colnames(data)
  if (target %in% cns) {
    y = data[, target]
    data[, target] = NULL
  }

  # if data.frame, transform to matrix so that following code executes correctly
  if (inherits(data, "data.frame"))
    data = as.matrix(data)

  # calculate fourier coefficients -> complex numbers
  ffttrafo = t(apply(data,1, fft))

  # extract amplitude or phase of fourier coefficients which are real numbers
  switch(fft.coeff,
         amplitude = {fftPA = getFourierAmplitude(fourier.comp = ffttrafo)},
         phase = {fftPA = getFourierPhase(fourier.comp = ffttrafo)}
         )

  if (!inherits(fftPA, "matrix")) {
    fftPA = as.data.frame(matrix(fftPA, nrow = 1))
  } else {
    fftPA = as.data.frame(fftPA)
  }

  # potentially include target again
  if (include.target)
    fftPA[, target] = y

  return(list(feat = fftPA, meta = NULL))
}

###
getFourierAmplitude = function(fourier.comp) {
  amp = apply(fourier.comp, 2, function(x) Re(x)^2 + Im(x)^2) # compute for each row
  amp = sqrt(amp)
  return(amp)
}

###
getFourierPhase = function(fourier.comp) {
  pha = apply(fourier.comp, 2, function(x) atan(Im(x) / Re(x)))
  return(pha)
}
