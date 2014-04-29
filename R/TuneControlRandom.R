#' @export
#' @rdname TuneControl
makeTuneControlRandom = function(same.resampling.instance = TRUE, maxit = 100L) {
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len = 1L, na.ok = FALSE)
  makeTuneControl(same.resampling.instance = same.resampling.instance, maxit = maxit,
    start = list(), cl = "TuneControlRandom")
}
