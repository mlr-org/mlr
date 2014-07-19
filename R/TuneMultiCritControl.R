#' @title Create control structures for tuning.
#'
#' @description
#' The following tuners are available:
#' \describe{
#'   \item{makeTuneControlGrid}{Grid search. All kinds of parameter types can be handled,
#'     but you have discretize them yourself by always using
#'     \code{\link[ParamHelpers]{makeDiscreteParam}} in the \code{par.set}
#'     passed to \code{\link{tuneParams}}.}
#'   \item{makeTuneControlRandom}{Random search. All kinds of parameter types can be handled.}
#' }
#'
#' Dependent parameters can currently only be handled by random search and irace.
#'
#' @inheritParams TuneControl
#' @return [\code{\link{TuneControl}}]. The specific subclass is one of
#'   \code{\link{TuneControlGrid}}, \code{\link{TuneControlRandom}},
#' @family tune_multicrit
#' @name TuneMultiCritControl
#' @rdname TuneMultiCritControl
#' @aliases TuneMultiCritControlGrid TuneMultiCritControlRandom
NULL

makeTuneMultiCritControl = function(same.resampling.instance, impute.val = Inf, ..., cl) {
  assertFlag(same.resampling.instance)
  assertNumber(impute.val)
  x = makeOptControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val, ...)
  addClasses(x, c(cl, "TuneMultiCritControl"))
}

#' @export
print.TuneMultiCritControl = function(x, ...) {
  catf("Tune multicrit control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  catf("Imputation value: %g", x$impute.val)
  catf("Further arguments: %s", convertToShortString(x$extra.args))
}

