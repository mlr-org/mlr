#' @title Result of multi-criteria tuning.
#'
#' @description
#' Container for results of hyperparameter tuning.
#' Contains the obtained pareto set and front
#' and the optimization path which lead there.
#'
#' Object members:
#' \describe{
#' \item{learner [\code{\link{Learner}}]}{Learner that was optimized.}
#' \item{control [\code{\link{TuneControl}}]}{Control object from tuning.}
#' \item{x [\code{list}]}{List of lists of non-dominated hyperparameter settings in pareto set.
#'   Note that when you have trafos on some of your params, \code{x} will always be
#'   on the TRANSFORMED scale so you directly use it.}
#' \item{y [\code{matrix}]}{Pareto front for \code{x}.}
#' \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path which lead to \code{x}.
#'   Note that when you have trafos on some of your params, the opt.path always contains the
#'   UNTRANSFORMED values on the original scale. You can simply call \code{trafoOptPath(opt.path)} to
#'   transform them, or, \code{as.data.frame{trafoOptPath(opt.path)}}}
#' }
#' @name TuneMultiCritResult
#' @rdname TuneMultiCritResult
NULL
makeTuneMultiCritResult = function(learner, ind, x, y, control, opt.path) {
  # set threshold to NULL, we can not currently tune for it in an MCO way
  or = makeOptResult(learner, control, x, y, NULL, opt.path, "TuneMultiCritResult")
  or$ind = ind
  return(or)
}

makeTuneMultiCritResultFromOptPath = function(learner, par.set, measures, control, opt.path) {
  j = getOptPathParetoFront(opt.path, index = TRUE)
  els = lapply(j, getOptPathEl, op = opt.path)
  xs = extractSubList(els, "x", simplify = FALSE)
  xs = lapply(xs, trafoValue, par = par.set)
  xs = lapply(xs, removeMissingValues)
  ys = extractSubList(els, "y", simplify = "rows")
  colnames(ys) = opt.path$y.names
  makeTuneMultiCritResult(learner, j, xs, ys, control, opt.path)
}


#'@export
print.TuneMultiCritResult = function(x, ...) {
  catf("Tune multicrit result:")
  catf("Points on front: %i", length(x$x))
}

