#' @export
#' @rdname TuneControl
makeTuneControlGrid = function(same.resampling.instance = TRUE, resolution = 10L) {
  makeTuneControl(same.resampling.instance = same.resampling.instance,
    start = list(), resolution = resolution, cl = "TuneControlGrid")
}
