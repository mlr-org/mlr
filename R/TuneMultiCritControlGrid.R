#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlGrid = function(same.resampling.instance = TRUE, resolution = 10L, log.fun = NULL) {
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance,
    resolution = resolution, log.fun = log.fun, cl = "TuneMultiCritControlGrid")
}

