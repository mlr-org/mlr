#' Result of feature selection.
#'
#' Container for results of feature selection.
#' Contains the obtained features, their performance values
#' and the optimization path which lead there.  \cr
#' You can visualize it using [analyzeFeatSelResult].
#'
#' Object members:
#' \describe{
#' \item{learner ([Learner])}{Learner that was optimized.}
#' \item{control ([FeatSelControl])}{ Control object from feature selection.}
#' \item{x ([character])}{Vector of feature names identified as optimal.}
#' \item{y ([numeric])}{Performance values for optimal `x`.}
#' \item{threshold ([numeric])}{Vector of finally found and used thresholds
#'   if `tune.threshold` was enabled in [FeatSelControl], otherwise not present and
#'   hence `NULL`.}
#' \item{opt.path ([ParamHelpers::OptPath])}{Optimization path which lead to `x`.}
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


