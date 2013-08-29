#' @import BBmisc
#' @import parallelMap
#' @import ParamHelpers
#' @importFrom stats predict
#' @importFrom codetools findGlobals

.onAttach <- function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package="mlr", levels="resample")
  parallelRegisterLevels(package="mlr", levels="tune")
}  