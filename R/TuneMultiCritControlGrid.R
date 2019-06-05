#' @export
#' @param resolution ([integer])\cr
#'   Resolution of the grid for each numeric/integer parameter in `par.set`.
#'   For vector parameters, it is the resolution per dimension.
#'   Either pass one resolution for all parameters, or a named vector.
#'   See [ParamHelpers::generateGridDesign].
#'   Default is 10.
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlGrid = function(same.resampling.instance = TRUE,
  resolution = 10L, log.fun = "default", final.dw.perc = NULL, budget = NULL) {
  resolution = asCount(resolution, positive = TRUE)
  makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance,
    resolution = resolution, log.fun = log.fun, final.dw.perc = final.dw.perc,
    budget = budget, cl = "TuneMultiCritControlGrid")
}
