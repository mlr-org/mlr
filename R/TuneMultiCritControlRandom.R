#' @param maxit [\code{integer(1)}]\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlRandom = function(same.resampling.instance = TRUE, maxit = 100L, log.fun = NULL) {
  maxit = asCount(maxit)
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance,
    maxit = maxit, log.fun = log.fun, cl = "TuneMultiCritControlRandom")
}

