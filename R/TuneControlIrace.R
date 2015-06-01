#' @param n.instances [\code{integer(1)}]\cr
#'   Number of random resampling instances for irace, see details.
#'   Default is 100.
#' @param show.irace.output [\code{logical(1)}]\cr
#'   Show console output of irace while tuning?
#'   Default is \code{FALSE}.
#' @export
#' @rdname TuneControl
makeTuneControlIrace = function(impute.val = NULL, n.instances = 100L, show.irace.output = FALSE,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, budget = NULL, ...) {

  # check, if 'budget' fits to 'maxExperiments'
  args = list(...)
  if (is.null(budget)) {
    if (is.null(args$maxExperiments))
      budget = args$maxExperiments = 1000L
    else
      budget = args$maxExperiments
  } else {
    if (is.null(args$maxExperiments))
      args$maxExperiments = budget
    else if (budget != args$maxExperiments)
      stopf("The number of experiments (maxExperiments = %i) differs from the given budget (budget = %i).",
        args$maxExperiments, budget)
  }

  args$maxExperiments = asCount(args$maxExperiments)
  n.instances = asCount(n.instances)
  makeTuneControl(same.resampling.instance = FALSE, impute.val = impute.val,
    n.instances = n.instances, show.irace.output = show.irace.output,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, budget, ..., cl = "TuneControlIrace")
}
