#' @export
#' @rdname TuneControl
makeTuneControlOptim = function(same.resampling.instance=TRUE, start, ...) {
  makeTuneControl(same.resampling.instance=same.resampling.instance,
    start=start, ..., cl="TuneControlOptim")
}
