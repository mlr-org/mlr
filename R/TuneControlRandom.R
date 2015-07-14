#' @param maxit [\code{integer(1)} | NULL]\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, maxit = 100L, tune.threshold = FALSE,
  tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, budget = NULL) {

  if (is.null(budget))
    budget = maxit
  else if (is.null(maxit))
    maxit = budget
  else if (budget != maxit)
    stopf("The parameters budget (%i) and maxit (%i) differ.", budget, maxit)
  maxit = asCount(maxit)

  makeTuneControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args, final.dw.perc = final.dw.perc, 
    log.fun = log.fun, budget = budget, cl = "TuneControlRandom")
}
