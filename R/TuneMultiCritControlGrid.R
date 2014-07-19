#' @inheritParams TuneControlGrid
#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlGrid = function(same.resampling.instance = TRUE, impute.val = Inf, resolution = 10L) {
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    resolution = resolution, cl = "TuneMultiCritControlGrid")
}

