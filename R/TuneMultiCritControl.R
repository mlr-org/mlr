#' @title Create control structures for multi-criteria tuning.
#'
#' @description
#' The following tuners are available:
#' \describe{
#'   \item{makeTuneControlGrid}{Grid search. All kinds of parameter types can be handled.
#'     You can either use their correct param type and \code{resolution},
#'     or discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}}
#'     in the \code{par.set} passed to \code{\link{tuneParams}}.}
#'   \item{makeTuneMultiCritControlRandom}{Random search. All kinds of parameter types can be handled.}
#'   \item{makeTuneMultiCritControlNSGA2}{Evolutionary method \code{\link[mco]{nsga2}}.
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.}
#' }
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

makeTuneMultiCritControl = function(measures, same.resampling.instance, impute.val = NULL, ..., cl) {
  assertFlag(same.resampling.instance)
  if (!is.null(impute.val))
    assertNumeric(impute.val, any.missing = FALSE)
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

