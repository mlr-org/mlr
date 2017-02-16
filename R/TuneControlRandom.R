#' @param maxit [\code{integer(1)} | NULL]\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, maxit = NULL, tune.threshold = FALSE,
  tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, budget = NULL) {

  # if we dont get neither budget nor maxit, set it to default, otherwise take one of the 2
  if (is.null(budget) && is.null(maxit))
    budget = maxit = 100L
  if (!is.null(maxit))
    maxit = asCount(maxit)
  if (!is.null(budget))
    budget = asCount(budget)
  maxit = coalesce(maxit, budget)
  budget = coalesce(budget, maxit)
  if (budget != maxit)
      stopf("The parameters budget (%i) and maxit (%i) differ.", budget, maxit)

  makeTuneControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args, final.dw.perc = final.dw.perc,
    log.fun = log.fun, budget = budget, cl = "TuneControlRandom")
}
