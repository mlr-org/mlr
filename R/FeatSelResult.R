#' Result of feature selection.
#'
#' Container for results of feature selection.
#' Contains the obtained features, their performance values
#' and the optimization path which lead there.  \cr
#' You can visualize it using \code{\link{analyzeFeatSelResult}}.
#'
#' Object members:
#' \describe{
#' \item{learner [\code{\link{Learner}}]}{Learner that was optimized.}
#' \item{control [\code{\link{FeatSelControl}}]}{ Control object from feature selection.}
#' \item{x [\code{character}]}{Vector of feature names identified as optimal.}
#' \item{y [\code{numeric}]}{Performance values for optimal \code{x}.}
#' \item{threshold [\code{numeric}]}{Vector of finally found and used thresholds
#'   if \code{tune.threshold} was enabled in \code{\link{FeatSelControl}}, otherwise not present and
#'   hence \code{NULL}.}
#' \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path which lead to \code{x}.}
#' }
#' @name FeatSelResult
#' @rdname FeatSelResult
NULL


makeFeatSelResult = function(learner, control, x, y, threshold, opt.path) {
  makeOptResult(learner, control, x, y, threshold, opt.path, "FeatSelResult")
}


#' @export
print.FeatSelResult = function(x, ...) {
  catf("FeatSel result:")

  n.feats = length(x$x)
  printed.features = 10L
  if (n.feats > printed.features)
    x = c(head(x, printed.features), "...")
  else
    x = head(x, printed.features)

  catf("Features (%i): %s", n.feats, collapse(x$x, ", "))
  if (!is.null(x$threshold))
    catf("Threshold: %s", collapse(sprintf("%2.2f", x$threshold)))
  catf("%s", perfsToString(x$y))
}

makeFeatSelResultFromOptPath = function(learner, measures, control, opt.path,
  dob = opt.path$env$dob, ties = "random") {

  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), dob = dob, ties = ties)
  e = getOptPathEl(opt.path, i)
  # if we had threshold tuning, get th from op and set it in result object
  threshold = if (control$tune.threshold) e$extra$threshold else NULL
  makeFeatSelResult(learner, control, names(e$x)[e$x == 1], e$y, threshold, opt.path)
}


