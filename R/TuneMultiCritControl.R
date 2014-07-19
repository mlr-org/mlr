#' @title Create control structures for multi-criteria tuning.
#'
#' @description
#' The following tuners are available:
#' \describe{
#'   \item{makeTuneMultiCritControlGrid}{Grid search. All kinds of parameter types can be handled,
#'     but you have discretize them yourself by always using
#'     \code{\link[ParamHelpers]{makeDiscreteParam}} in the \code{par.set}
#'     passed to \code{\link{tuneParams}}.}
#'   \item{makeTuneMultiCritControlRandom}{Random search. All kinds of parameter types can be handled.}
#'   \item{makeTuneMultiCritControlNSGA2}{Evolutionary method \code{\link[mco]{nsga2}}.
#'     Can handle numeric(vector) and integer(vector) hyperparameters.
#'     For integers the internally proposed numeric values are automatically rounded.}
#' }
#'
#' Dependent parameters can currently only be handled by random search and grid search.
#'
#' @inheritParams TuneControl
#' @return [\code{\link{TuneMultiCritControl}}]. The specific subclass is one of
#'   \code{\link{TuneMultiCritControlGrid}}, \code{\link{TuneMultiCritControlRandom}},
#'   \code{\link{TuneMultiCritControlNSGA2}}.
#' @family tune_multicrit
#' @name TuneMultiCritControl
#' @rdname TuneMultiCritControl
#' @aliases TuneMultiCritControlGrid TuneMultiCritControlRandom TuneMultiCritControlNSGA2
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

