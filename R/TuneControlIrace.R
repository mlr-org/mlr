#' @title Create control object for hyperparameter tuning with Irace.
#'
#' @description
#' Tuning with iterated F-Racing with method [irace::irace].
#' All kinds of parameter types can be handled. We return the best of the final elite
#' candidates found by irace in the last race. Its estimated performance is the mean of all
#' evaluations ever done for that candidate. More information on irace can be found in the TR at
#' <http://iridia.ulb.ac.be/IridiaTrSeries/link/IridiaTr2011-004.pdf>.
#'
#' For resampling you have to pass a [ResampleDesc],
#' not a [ResampleInstance].
#' The resampling strategy is randomly instantiated `n.instances` times and
#' these are the instances in the sense of irace (`instances` element of `tunerConfig`
#' in [irace::irace]). Also note that irace will always
#' store its tuning results in a file on disk, see the package documentation for details on this
#' and how to change the file path.
#'
#' @inherit TuneControl
#' @param budget (`integer(1)`)\cr
#'   Maximum budget for tuning. This value restricts the number of function
#'   evaluations. It is passed to `maxExperiments`.
#' @param n.instances (`integer(1)`)\cr
#'   Number of random resampling instances for irace, see details.
#'   Default is 100.
#' @param show.irace.output (`logical(1)`)\cr
#'   Show console output of irace while tuning?
#'   Default is `FALSE`.
#' @return ([TuneControlIrace])
#' @aliases TuneControlIrace
#' @family tune
#' @export
makeTuneControlIrace = function(impute.val = NULL, n.instances = 100L,
  show.irace.output = FALSE, tune.threshold = FALSE, tune.threshold.args = list(),
  log.fun = "default", final.dw.perc = NULL, budget = NULL, ...) {

  n.instances = asCount(n.instances)

  # construct super object so we get arg checks
  x = makeTuneControl(same.resampling.instance = FALSE, impute.val = impute.val,
    n.instances = n.instances, show.irace.output = show.irace.output,
    start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget, ..., cl = "TuneControlIrace")

  # argcheck maxExperiments
  if (!is.null(x$extra.args$maxExperiments)) {
    x$extra.args$maxExperiments = asCount(x$extra.args$maxExperiments)
  }

  # check that budget and maxExperiments are the same if both given
  if (!is.null(budget) && !is.null(x$extra.args$maxExperiments) && budget != x$extra.args$maxExperiments) {
    stopf("The number of experiments (maxExperiments = %i) differs from the given budget (budget = %i).",
      x$extra.args$maxExperiments, budget)
  }
  # now if budget was given, use it
  if (!is.null(budget)) {
    x$extra.args$maxExperiments = x$budget
  }

  return(x)
}
