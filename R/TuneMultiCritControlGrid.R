#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlGrid = function(same.resampling.instance = TRUE, resolution = 10L) {
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance,
    resolution = resolution, cl = "TuneMultiCritControlGrid")
}

