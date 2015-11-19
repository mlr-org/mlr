#' @import BBmisc
#' @import checkmate
#' @import ggplot2
#' @import graphics
#' @import methods
#' @import parallelMap
#' @import ParamHelpers
#' @import plyr
#' @import reshape2
#' @import stats
#' @import utils
#' @importFrom survival Surv
#' @importFrom survival is.Surv

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams"))
}


#' @rdname makeResampleDesc
#' @aliases NULL
#' @docType NULL
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @section Standard ResampleDesc objects:
#' For common resampling strategies you can save some typing
#' by using the following description objects:
#' \describe{
#' \item{hout}{holdout a.k.a. test sample estimation
#' (two-thirds training set, one-third testing set)}
#' \item{cv2}{2-fold cross-validation}
#' \item{cv3}{3-fold cross-validation}
#' \item{cv5}{5-fold cross-validation}
#' \item{cv10}{10-fold cross-validation}
#' }

hout = makeResampleDesc("Holdout")
cv2 = makeResampleDesc("CV", iters = 2L)
cv3 = makeResampleDesc("CV", iters = 3L)
cv5 = makeResampleDesc("CV", iters = 5L)
cv10 = makeResampleDesc("CV", iters = 10L)
