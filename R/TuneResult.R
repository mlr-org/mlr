#' @title Result of tuning.
#'
#' @description
#' Container for results of hyperparameter tuning.
#' Contains the obtained point in search space, its performance values
#' and the optimization path which lead there.
#'
#' Object members:
#' \describe{
#' \item{learner ([Learner])}{Learner that was optimized.}
#' \item{control ([TuneControl])}{Control object from tuning.}
#' \item{x ([list])}{Named list of hyperparameter values identified as optimal.
#'   Note that when you have trafos on some of your params, `x` will always be
#'   on the TRANSFORMED scale so you directly use it.}
#' \item{y ([numeric])}{Performance values for optimal `x`.}
#' \item{threshold ([numeric])}{Vector of finally found and used thresholds
#'   if `tune.threshold` was enabled in [TuneControl], otherwise not present and
#'   hence `NULL`.}
#' \item{opt.path ([ParamHelpers::OptPath])}{Optimization path which lead to `x`.
#'   Note that when you have trafos on some of your params, the opt.path always contains the
#'   UNTRANSFORMED values on the original scale. You can simply call `trafoOptPath(opt.path)` to
#'   transform them, or, \code{as.data.frame{trafoOptPath(opt.path)}}.
#'   If mlr option `on.error.dump` is `TRUE`, `OptPath` will have a `.dump` object
#'   in its `extra` column which contains error dump traces from failed optimization evaluations.
#'   It can be accessed by `getOptPathEl(opt.path)$extra$.dump`.}
#' }
#' @name TuneResult
#' @rdname TuneResult
NULL
makeTuneResult = function(learner, control, x, y, resampling, threshold, opt.path, ...) {
  makeOptResult(learner, control, x, y, resampling, threshold, opt.path, "TuneResult", ...)
}

makeTuneResultFromOptPath = function(learner, par.set, measures, resampling, control, opt.path) {
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties = "random")
  e = getOptPathEl(opt.path, i)
  x = trafoValue(par.set, e$x)
  x = removeMissingValues(x)
  threshold = if (control$tune.threshold) getThresholdFromOptPath(opt.path, i) else NULL
  makeTuneResult(learner, control, x, e$y, resampling, threshold, opt.path)
}


#' @export
print.TuneResult = function(x, ...) {
  catf("Tune result:")
  catf("Op. pars: %s", paramValueToString(x$opt.path$par.set, x$x))
  if (!is.null(x$threshold)) {
    catf("Threshold: %s", collapse(sprintf("%2.2f", x$threshold)))
  }
  catf("%s", perfsToString(x$y))
}
