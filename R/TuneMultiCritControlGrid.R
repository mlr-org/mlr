#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlGrid = function(same.resampling.instance = TRUE,
  resolution = 10L, log.fun = NULL, final.dw.perc = NULL, budget = NULL) {

  resolution = asCount(resolution, positive = TRUE)
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance,
    resolution = resolution, log.fun = log.fun, final.dw.perc = final.dw.perc,
    budget = budget, cl = "TuneMultiCritControlGrid")
}

