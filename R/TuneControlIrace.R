#' @param n.instances [\code{integer(1)}]\cr
#'   Number of random resampling instances for irace, see details.
#'   Default is 100.
#' @param show.irace.output [\code{logical(1)}]\cr
#'   Show console output of irace while tuning?
#'   Default is \code{FALSE}.
#' @export
#' @rdname TuneControl
makeTuneControlIrace = function(impute.val = NULL, n.instances = 100L,
  show.irace.output = FALSE, tune.threshold = FALSE, tune.threshold.args = list(),
  log.fun = NULL, final.dw.perc = NULL, budget = NULL, ...) {

  n.instances = asCount(n.instances)

  # construct super object so we get arg checks
  x = makeTuneControl(same.resampling.instance = FALSE, impute.val = impute.val,
    n.instances = n.instances, show.irace.output = show.irace.output,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget, ..., cl = "TuneControlIrace")

  # argcheck maxExperiments
  if (!is.null(x$extra.args$maxExperiments))
    x$extra.args$maxExperiments = asCount(x$extra.args$maxExperiments)

  # check that budget and maxExperiments are the same if both given
  if (!is.null(budget) && !is.null(x$extra.args$maxExperiments) && budget != x$extra.args$maxExperiments)
    stopf("The number of experiments (maxExperiments = %i) differs from the given budget (budget = %i).",
      x$extra.args$maxExperiments, budget)
  # now if budget was given, use it
  if (!is.null(budget))
    x$extra.args$maxExperiments = x$budget

  return(x)
}
