#' @param maxit [\code{integer(1)}]\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, maxit = 100L) {
  maxit = convertInteger(maxit)
  assertInteger(maxit, len = 1L, any.missing = FALSE)
  makeTuneControl(same.resampling.instance = same.resampling.instance, maxit = maxit,
    start = list(), cl = "TuneControlRandom")
}
