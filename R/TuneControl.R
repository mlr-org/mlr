#' @title Create control structures for tuning.
#'
#' @description
#' The following tuners are available:
#' \describe{
#'   \item{makeTuneControlGrid}{Grid search. All kinds of parameter types can be handled.
#'     You can either use their correct param type and \code{resolution},
#'     or discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}}
#'     in the \code{par.set} passed to \code{\link{tuneParams}}.}
#'   \item{makeTuneControlRandom}{Random search. All kinds of parameter types can be handled.}
#'   \item{makeTuneControlCMAES}{CMA Evolution Strategy with method \code{\link[cmaes]{cma_es}}.
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.
#'     The sigma variance parameter is initialized to 1/4 of the span of box-constraints per
#'     parameter dimension.}
#'   \item{makeTuneControlGenSA}{Generalized simulated annealing with method \code{\link[GenSA]{GenSA}}.
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.}
#'   \item{makeTuneControlIrace}{Tuning with iterated F-Racing with method \code{\link[irace]{irace}}.
#'     All kinds of parameter types can be handled. We return the best of the final elite
#'     candidates found by irace in the last race. Its estimated performance is the mean of all
#'     evaluations ever done for that candidate.}
#' }
#'
#' Some notes on irace: For resampling you have to pass a \code{\link{ResampleDesc}},
#' not a \code{\link{ResampleInstance}}.
#' The resampling strategy is randomly instantiated \code{n.instances} times and
#' these are the instances in the sense of irace (\code{instances} element of \code{tunerConfig}
#' in \code{\link[irace]{irace}}). Also note that irace will always
#' store its tuning results in a file on disk, see the package documentation for details on this
#' and how to change the file path.
#'
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is \code{TRUE}.
#' @template arg_imputey
#' @param start [\code{list}]\cr
#'   Named list of initial parameter values.
#' @param tune.threshold [\code{logical(1)}]\cr
#'   Should the threshold be tuned for the measure at hand, after each hyperparameter evaluation,
#'   via \code{\link{tuneThreshold}}?
#'   Only works for classification if the predict type is \dQuote{prob}.
#'   Default is \code{FALSE}.
#' @param log.fun [\code{function} | \code{NULL}]\cr
#'   Function used for logging. If set to \code{NULL}, the internal default will be used.
#'   Otherwise a function with arguments \code{learner}, \code{resampling}, \code{measures},
#'   \code{par.set}, \code{control}, \code{opt.path}, \code{dob}, \code{x}, \code{y}, \code{remove.nas},
#'   and \code{stage} is expected. See the implementation for details.
#' @param ... [any]\cr
#'   Further control parameters passed to the \code{control} argument of \code{\link[cmaes]{cma_es}} and
#'   \code{tunerConfig} argument of \code{\link[irace]{irace}}.
#' @return [\code{\link{TuneControl}}]. The specific subclass is one of
#'   \code{\link{TuneControlGrid}}, \code{\link{TuneControlRandom}},
#'   \code{\link{TuneControlCMAES}}, \code{\link{TuneControlGenSA}},
#'   \code{\link{TuneControlIrace}}.
#' @family tune
#' @name TuneControl
#' @rdname TuneControl
#' @aliases TuneControlGrid TuneControlRandom TuneControlCMAES TuneControlGenSA TuneControlIrace
NULL

makeTuneControl = function(same.resampling.instance, impute.val = NULL, start = NULL, tune.threshold = FALSE, log.fun = NULL, ..., cl) {
  if (!is.null(start))
    assertList(start, min.len = 1L, names = "unique")
  x = makeOptControl(same.resampling.instance, impute.val, tune.threshold, log.fun, ...)
  x$start = start
  x$log.fun = log.fun
  addClasses(x, c(cl, "TuneControl"))
}

#' @export
print.TuneControl = function(x, ...) {
  catf("Tune control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  catf("Imputation value: %g", x$impute.val)
  catf("Start: %s", convertToShortString(x$start))
  catf("Tune threshold: %s", x$tune.threshold)
  catf("Further arguments: %s", convertToShortString(x$extra.args))
}
