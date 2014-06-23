#' @export
#' @rdname TuneControl
makeTuneControlCMAES = function(same.resampling.instance = TRUE, start = NULL, ...) {
  makeTuneControl(same.resampling.instance = same.resampling.instance,
    start = start, ..., cl = "TuneControlCMAES")
}
