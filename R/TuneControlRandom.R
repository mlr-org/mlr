#' @title Create control object for hyperparameter tuning with random search.
#'
#' @description
#' Random search. All kinds of parameter types can be handled.
#'
#' @inherit TuneControl
#' @param budget (`integer(1)`)\cr
#'   Maximum budget for tuning. This value restricts the number of function
#'   evaluations. The `budget` equals the number of iterations (`maxit`) performed by
#'   the random search algorithm.
#' @param maxit (`integer(1)` | NULL)\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @return ([TuneControlRandom])
#' @aliases TuneControlRandom
#' @family tune
#' @export
makeTuneControlRandom = function(same.resampling.instance = TRUE, maxit = NULL, tune.threshold = FALSE,
  tune.threshold.args = list(), log.fun = "default", final.dw.perc = NULL, budget = NULL) {

  # if we dont get neither budget nor maxit, set it to default, otherwise take one of the 2
  if (is.null(budget) && is.null(maxit)) {
    budget = maxit = 100L
  }
  if (!is.null(maxit)) {
    maxit = asCount(maxit)
  }
  if (!is.null(budget)) {
    budget = asCount(budget)
  }
  maxit = coalesce(maxit, budget)
  budget = coalesce(budget, maxit)
  if (budget != maxit) {
    stopf("The parameters budget (%i) and maxit (%i) differ.", budget, maxit)
  }

  makeTuneControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args, final.dw.perc = final.dw.perc,
    log.fun = log.fun, budget = budget, cl = "TuneControlRandom")
}
