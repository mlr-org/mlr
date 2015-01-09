#' @import BBmisc
#' @import checkmate
#' @import ggplot2
#' @import parallelMap
#' @import ParamHelpers
#' @import plyr
#' @importFrom stats predict
#' @importFrom survival Surv
#' @importFrom survival is.Surv

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams"))
}
