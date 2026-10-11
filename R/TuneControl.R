#' @title Control object for tuning
#'
#' @description General tune control object. See the tuning control constructors
#' for their arguments and optimizer-specific settings.
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
