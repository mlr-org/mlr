#' Result of feature selection.
#' 
#' Container for results of feature selection.
#' Contains the obtained features, their performance values
#' and the optimization path which lead there. 
#'
#' Object members:
#' \describe{
#' \item{learner [\code{\link{Learner}}]}{Learner that was optimized.}
#' \item{control [\code{\link{FeatSelControl}}]}{ Control object from feature selection.}
#' \item{x [\code{character}]}{Vector of feature names identified as optimal.}
#' \item{y [\code{numeric}]}{Performance values for optimal \code{x}.}
#' \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path which lead to \code{x}.}
#' }
#' @name FeatSelResult
#' @rdname FeatSelResult
NULL

makeFeatSelResult = function(learner, control, x, y, opt.path) {
  makeOptResult(learner, control, x, y, opt.path, "FeatSelResult")
}


#' @S3method print FeatSelResult
print.FeatSelResult = function(x, ...) {
  catf("FeatSel result:")
  n.feats = length(x$x)
  printed.features = 10
  if(length(x$x) > printed.features) {
    catf("Features (%i): %s", n.feats, paste(c(x$x[1:printed.features], "..."), collapse = ", "))
  } else {
    catf("Features (%i): %s", n.feats, paste(x$x[1:n.feats], collapse = ", "))
  }
  catf("%s", perfsToString(x$y))
}
