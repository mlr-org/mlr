#' @title Control object for tuning
#'
#' @description General tune control object.
#' @param same.resampling.instance (`logical(1)`)\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is `TRUE`.
#' @template arg_imputey
#' @param start ([list])\cr
#'   Named list of initial parameter values.
#' @param tune.threshold (`logical(1)`)\cr
#'   Should the threshold be tuned for the measure at hand, after each hyperparameter evaluation,
#'   via [tuneThreshold]?
#'   Only works for classification if the predict type is \dQuote{prob}.
#'   Default is `FALSE`.
#' @param tune.threshold.args ([list])\cr
#'   Further arguments for threshold tuning that are passed down to [tuneThreshold].
#'   Default is none.
#' @template arg_log_fun
#' @param final.dw.perc (`boolean`)\cr
#'   If a Learner wrapped by a [makeDownsampleWrapper] is used, you can define the value of `dw.perc` which is used to train the Learner with the final parameter setting found by the tuning.
#'   Default is `NULL` which will not change anything.
#' @param ... (any)\cr
#'   Further control parameters passed to the `control` arguments of
#'   [cmaes::cma_es] or [GenSA::GenSA], as well as
#'   towards the `tunerConfig` argument of [irace::irace].
#' @name TuneControl
#' @rdname TuneControl
#' @family tune
NULL

makeTuneControl = function(same.resampling.instance, impute.val = NULL,
  start = NULL, tune.threshold = FALSE, tune.threshold.args = list(),
  log.fun = "default", final.dw.perc = NULL, budget = NULL, ..., cl) {

  if (!is.null(start)) {
    assertList(start, min.len = 1L, names = "unique")
  }
  if (identical(log.fun, "default")) {
    log.fun = logFunTune
  } else if (identical(log.fun, "memory")) {
    log.fun = logFunTuneMemory
  }
  if (!is.null(budget)) {
    budget = asCount(budget)
  }
  if (!is.null(final.dw.perc)) {
    assertNumeric(final.dw.perc, lower = 0, upper = 1)
  }
  x = makeOptControl(same.resampling.instance, impute.val, tune.threshold, tune.threshold.args, log.fun, final.dw.perc, ...)
  x$start = start
  x$budget = budget
  addClasses(x, c(cl, "TuneControl"))
}

#' @export
print.TuneControl = function(x, ...) {
  catf("Tune control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  catf("Imputation value: %s", ifelse(is.null(x$impute.val), "<worst>", sprintf("%g", x$impute.val)))
  catf("Start: %s", convertToShortString(x$start))
  catf("Budget: %i", x$budget)
  catf("Tune threshold: %s", x$tune.threshold)
  catf("Further arguments: %s", convertToShortString(x$extra.args))
}
