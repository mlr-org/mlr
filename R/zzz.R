#' @import BBmisc
#' @import parallelMap
#' @import ParamHelpers
#' @importFrom stats predict
#' @importFrom codetools findGlobals
#' @importFrom survival Surv

.onAttach <- function(libname, pkgname) {
  configureMlr()
  # parallelRegisterLevels(package="mlr", levels=c("resample", "tuneParams", "selectFeatures"))
}
