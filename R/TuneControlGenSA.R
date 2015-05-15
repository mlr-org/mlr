#' @export
#' @rdname TuneControl
makeTuneControlGenSA = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, budget = 500L, ...) {

  args = list(...)
  if (!is.null(args$maxit) && (args$maxit != budget)) {
    args$maxit = asCount(budget)
    warningf("Considering the given budget, the number of generations (maxit = %i) was changed to %i.",
      args$maxit, budget)
  }

  default = list(smooth = FALSE)
  default = list()
  args = insert(default, args)
  args2 = list(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = start, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, cl = "TuneControlGenSA", budget = budget)
  do.call(makeTuneControl, c(args, args2))
}
