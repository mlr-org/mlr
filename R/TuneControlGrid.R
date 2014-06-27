#' @param resolution [\code{integer}]\cr
#'   Resolution of the grid for each numeric/integer parameter in \code{par.set}.
#'   For vector parameters, it is the resolution per dimension.
#'   Either pass one resolution for all parameters, or a named vector.
#'   See \code{\link[ParamHelpers]{generateGridDesign}}.
#'   Default is 10.
#' @export
#' @rdname TuneControl
makeTuneControlGrid = function(same.resampling.instance = TRUE, impute.val = Inf, resolution = 10L) {
  makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, resolution = resolution, cl = "TuneControlGrid")
}
