#' @import methods
#' @import BBmisc
#' @import parallelMap
#' @import ParamHelpers
#' @import checkmate
#' @importFrom stats predict
#' @importFrom codetools findGlobals
#' @importFrom survival Surv
#' @importFrom survival is.Surv

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams"))
}
