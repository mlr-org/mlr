#' @export
#' @rdname TuneControl
makeTuneControlCMAES = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, budget = 100L, ...) {

  ctrl = makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = start, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, ..., cl = "TuneControlCMAES", budget = budget)

  # assure that the CMAES does not exceed the given budget
  if (is.null(ctrl$extra.args$lambda)) {
    ctrl$extra.args$lambda = as.integer(4 + floor(3 * log(length(start))))
  }
  ctrl$extra.args$maxit = as.integer(floor(budget / ctrl$extra.args$lambda))
  if (!is.null(list(...)$maxit) && (list(...)$maxit != ctrl$extra.args$maxit))
    warningf("Considering the given budget, the number of generations (maxit = %i) was changed to %i.",
      list()$maxit, ctrl$extra.args$maxit)

  # assure that the budget is big enough for at least one generation
  if(ctrl$extra.args$maxit == 0)
    stopf("The current budget (%i) is too small. It should at least have the size of one population (lambda = %i)!",
      budget, ctrl$extra.args$lambda)
  ctrl
}
