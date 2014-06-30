#' @param maxit [\code{integer(1)}]\cr
#'   Number of iterations for random search.
#'   Default is 100.
#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, impute.val = Inf, maxit = 100L) {
  maxit = asCount(maxit)
  makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    maxit = maxit, start = NULL, cl = "TuneControlRandom")
}
