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

#' @export
print.FeatSelResult = function(x, ...) {
  catf("FeatSel result:")

  shortenX = function(x) {
    clipString(collapse(x, ", "), 50L)
  }

  if (!isTRUE(all.equal(x$x.bit.names, x$x))) {
    catf("Bits (%i): %s", length(x$x.bit.names), shortenX(x$x.bit.names))
  }
  catf("Features (%i): %s", length(x$x), shortenX(x$x))
  if (!is.null(x$threshold)) {
    catf("Threshold: %s", collapse(sprintf("%2.2f", x$threshold)))
  }
  catf("%s", perfsToString(x$y))
}

makeFeatSelResultFromOptPath = function(learner, measures, resampling, control, opt.path, dob = opt.path$env$dob, ties = "random", task, bits.to.features) {
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), dob = dob, ties = ties)
  e = getOptPathEl(opt.path, i)
  # if we had threshold tuning, get th from op and set it in result object
  threshold = if (control$tune.threshold) e$extra$threshold else NULL
  x.bits = unlist(e$x)
  x.bit.names = names(e$x)[e$x == 1]
  x = bits.to.features(x.bits, task)
  makeOptResult(learner, control, x, e$y, resampling, threshold, opt.path, "FeatSelResult", x.bit.names = x.bit.names)
}
