#' @param n.instances [\code{integer(1)}]\cr
#'   Number of random resampling instances for irace, see details.
#'   Default is 100.
#' @param show.irace.output [\code{logical(1)}]\cr
#'   Show console output of irace while tuning?
#'   Default is \code{FALSE}.
#' @export
#' @rdname TuneControl
makeTuneControlIrace = function(impute.val = NULL, n.instances = 100L, show.irace.output = FALSE,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, budget = 1000L, ...) {

  # can't handle usage of 'budget' and 'maxExperiments' (if their values differ)
  if ("maxExperiments" %in% names(list(...)))
    if (list(...)$maxExperiments != budget)
      stopf("The number of experiments (maxExperiments = %i) differs from the given budget (%i).",
        list(...)$maxExperiments, budget)

  n.instances = asCount(n.instances)
  makeTuneControl(same.resampling.instance = FALSE, impute.val = impute.val,
    n.instances = n.instances, show.irace.output = show.irace.output,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, budget = budget, ..., cl = "TuneControlIrace")
}
