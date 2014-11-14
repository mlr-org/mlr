#' @param maxit [\code{integer(1)}]\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, maxit = 100L,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL) {

  maxit = asCount(maxit)
  makeTuneControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, start = NULL, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, cl = "TuneControlRandom")
}
