#' @import methods
#' @importFrom survival Surv is.Surv
#' @importFrom graphics hist
#' @importFrom utils browseURL capture.output combn data getFromNamespace getS3method head methods tail
#' @import BBmisc
#' @import checkmate
#' @import parallelMap
#' @import ParamHelpers
#' @import ggplot2
#' @import stats
#' @import stringi
#' @import data.table

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlr", levels = c("benchmark", "resample", "selectFeatures", "tuneParams"))
}


#' @rdname makeResampleDesc
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
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
hout = makeResampleDesc("Holdout")

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv2 = makeResampleDesc("CV", iters = 2L)

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv3 = makeResampleDesc("CV", iters = 3L)

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv5 = makeResampleDesc("CV", iters = 5L)

#' @rdname makeResampleDesc
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
cv10 = makeResampleDesc("CV", iters = 10L)
