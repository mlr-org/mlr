#' @export
#' @rdname TuneControl
makeTuneControlCMAES = function(same.resampling.instance = TRUE, impute.val = NULL,
  start = NULL, tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL,
  final.dw.perc = NULL, budget = NULL, ...) {

  ctrl = makeTuneControl(same.resampling.instance = same.resampling.instance,
    impute.val = impute.val, start = start, tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args, log.fun = log.fun,
    final.dw.perc = final.dw.perc, budget = budget, ..., cl = "TuneControlCMAES")

  # use length(start) to define lambda in case it's not given
  if (is.null(ctrl$extra.args$lambda) && !is.null(start)) {
    ctrl$extra.args$lambda = as.integer(4 + floor(3 * log(length(start))))
    if (!is.null(budget) && (ctrl$extra.args$lambda > budget))
      stopf("The 'budget' needs to be at least %i.", ctrl$extra.args$lambda)
  }

  # if two of the three arguments budget, lambda and maxit are given,
  # the third one will be computed based on budget = lambda * maxit
  if (!is.null(ctrl$extra.args$lambda) && !is.null(ctrl$extra.args$maxit)) {
    ctrl$extra.args$lambda = asCount(ctrl$extra.args$lambda)
    ctrl$extra.args$maxit = asCount(ctrl$extra.args$maxit)
    if (is.null(budget)) {
      ctrl$budget = ctrl$extra.args$lambda * ctrl$extra.args$maxit
    } else if (ctrl$budget != ctrl$extra.args$lambda * ctrl$extra.args$maxit)
      stopf("budget (%i) != lambda (%i) * maxit (%i)",
        ctrl$budget, ctrl$extra.args$lambda, ctrl$extra.args$maxit)
  } else if (!is.null(ctrl$extra.args$lambda) && !is.null(budget)) {
    ctrl$extra.args$lambda = asCount(ctrl$extra.args$lambda)
    if (budget < ctrl$extra.args$lambda)
      stopf("'lambda' (%i) should not exceed 'budget' (%i)",
        ctrl$extra.args$lambda, budget)
      ctrl$extra.args$maxit = as.integer(budget %/% ctrl$extra.args$lambda)
  } else if (!is.null(ctrl$extra.args$maxit) && !is.null(budget)) {
    ctrl$extra.args$maxit = asCount(ctrl$extra.args$maxit)
    if (budget < ctrl$extra.args$maxit)
      stopf("'maxit' (%i) should not exceed 'budget' (%i)",
        ctrl$extra.args$maxit, budget)
    ctrl$extra.args$lambda = as.integer(budget %/% ctrl$extra.args$maxit)
  }

  return(ctrl)
}
