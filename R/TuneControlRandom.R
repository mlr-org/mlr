#' @param maxit [\code{integer(1)} | NULL]\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @param time.budget [\code{integer(1)} | NULL]\cr
#'   Running time budget in seconds. Note that the actual mbo run can take more time since
#'   the condition is checked after each iteration.
#' @param exec.time.budget [\code{integer(1)} | NULL]\cr
#'   Execution time (time spent executing the function passed to \code{mbo}) budget in seconds. Note that the actual mbo run can take more time since
#'   the condition is checked after each iteration.
#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, maxit = 100L,
  time.budget = NULL, exec.time.budget = NULL, tune.threshold = FALSE,
  tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, budget = NULL) {

  if (is.null(budget))
    budget = maxit
  else if (is.null(maxit))
    maxit = budget
  else if (budget != maxit)
    stopf("The parameters budget (%i) and maxit (%i) differ.", budget, maxit)
  maxit = asCount(maxit)

  if (is.null(time.budget)) {
    time.budget = Inf
  } else {
    assertCount(time.budget, na.ok = FALSE, positive = TRUE)
  }

  if (is.null(exec.time.budget)) {
    exec.time.budget = Inf
  } else {
    assertCount(exec.time.budget, na.ok = FALSE, positive = TRUE)
  }

  makeTuneControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, time.budget = time.budget, exec.time.budget = exec.time.budget, start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args, final.dw.perc = final.dw.perc, 
    log.fun = log.fun, budget = budget, cl = "TuneControlRandom")
}
