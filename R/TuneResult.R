#' Result of tuning.
#'
#' Container for results of hyperparameter tuning.
#' Contains the obtained point in search space, its performance values
#' and the optimization path which lead there.
#'
#' Object members:
#' \describe{
#' \item{learner [\code{\link{Learner}}]}{Learner that was optimized.}
#' \item{control [\code{\link{TuneControl}}]}{ Control object from tuning.}
#' \item{x [\code{list}]}{Named list of hyperparameter values identified as optimal.}
#' \item{y [\code{numeric}]}{Performance values for optimal \code{x}.}
#' \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path which lead to \code{x}.}
#' }
#' @name TuneResult
#' @rdname TuneResult
NULL
makeTuneResult = function(learner, control, x, y, opt.path) {
  makeOptResult(learner, control, x, y, opt.path, "TuneResult")
}


#'@export
print.TuneResult = function(x, ...) {
  catf("Tune result:")
  catf("Op. pars: %s", paramValueToString(x$opt.path$par.set, x$x))
  catf("%s", perfsToString(x$y))
}
