#' @title Create control structures for multi-criteria tuning.
#'
#' @description The following tuners are available:
#' \describe{
#'   \item{makeTuneMultiCritControlGrid}{Grid search. All kinds of parameter types can be handled.
#'     You can either use their correct param type and `resolution`,
#'     or discretize them yourself by always using [ParamHelpers::makeDiscreteParam]
#'     in the `par.set` passed to [tuneParams].}
#'   \item{makeTuneMultiCritControlRandom}{Random search. All kinds of parameter types can be handled.}
#'   \item{makeTuneMultiCritControlNSGA2}{Evolutionary method [mco::nsga2].
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.}
#'   \item{makeTuneMultiCritControlMBO}{Model-based/ Bayesian optimization. All kinds of
#'     parameter types can be handled.}
#' }
#'
#' @inheritParams TuneControl
#' @param budget (`integer(1)`)\cr
#'   Maximum budget for tuning. This value restricts the number of function
#'   evaluations. In case of `makeTuneMultiCritControlGrid` this number
#'   must be identical to the size of the grid. For
#'   `makeTuneMultiCritControlRandom` the `budget` equals the number
#'   of iterations (`maxit`) performed by the random search algorithm.
#'   In case of `makeTuneMultiCritControlNSGA2` the `budget`
#'   corresponds to the product of the maximum number of generations
#'   (`max(generations)`) + 1 (for the initial population) and the size of
#'   the population (`popsize`). For `makeTuneMultiCritControlMBO` the
#'   `budget` equals the number of objective function evaluations, i.e. the
#'   number of MBO iterations + the size of the initial design. If not `NULL`,
#'   this will overwrite existing stopping conditions in `mbo.control`.
#' @return ([TuneMultiCritControl]). The specific subclass is one of
#'   [TuneMultiCritControlGrid], [TuneMultiCritControlRandom],
#'   [TuneMultiCritControlNSGA2], [TuneMultiCritControlMBO].
#' @family tune_multicrit
#' @name TuneMultiCritControl
#' @rdname TuneMultiCritControl
#' @aliases TuneMultiCritControlGrid TuneMultiCritControlRandom TuneMultiCritControlNSGA2 TuneMultiCritControlMBO
NULL

makeTuneMultiCritControl = function(measures, same.resampling.instance,
  impute.val = NULL, log.fun = "default", final.dw.perc = NULL, budget = NULL, ..., cl) {
  assertFlag(same.resampling.instance)
  if (!is.null(impute.val)) {
    assertNumeric(impute.val, any.missing = FALSE)
  }
  if (identical(log.fun, "default")) {
    log.fun = logFunTune
  } else if (identical(log.fun, "memory")) {
    log.fun = logFunTuneMemory
  }
  if (!is.null(budget)) {
    budget = asCount(budget)
  }
  x = makeOptControl(same.resampling.instance, impute.val, log.fun = log.fun,
    final.dw.perc = final.dw.perc, ...)
  x$budget = budget
  addClasses(x, c(cl, "TuneMultiCritControl"))
}

#' @export
print.TuneMultiCritControl = function(x, ...) {
  catf("Tune multicrit control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  catf("Imputation value: %s", ifelse(is.null(x$impute.val), "<worst>", collapse(sprintf("%g", x$impute.val))))
  catf("Budget: %i", x$budget)
  catf("Further arguments: %s", convertToShortString(x$extra.args))
}
