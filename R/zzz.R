#' @import BBmisc
#' @import parallelMap
#' @import ParamHelpers
#' @importFrom stats predict
#' @importFrom codetools findGlobals

.onAttach <- function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package="mlr", levels=c("resample", "tune"))
}  
