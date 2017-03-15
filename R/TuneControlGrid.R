#' @title Create control object for hyperparameter tuning with grid search.
#'
#' @description
#' A basic grid search can handle all kinds of parameter types.
#' You can either use their correct param type and \code{resolution},
#' or discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}}
#' in the \code{par.set} passed to \code{\link{tuneParams}}.
#'
#' @inherit TuneControl
#' @param resolution [\code{integer}]\cr
#'   Resolution of the grid for each numeric/integer parameter in \code{par.set}.
#'   For vector parameters, it is the resolution per dimension.
#'   Either pass one resolution for all parameters, or a named vector.
#'   See \code{\link[ParamHelpers]{generateGridDesign}}.
#'   Default is 10.
#' @param budget [\code{integer(1)}]\cr
#'   Maximum budget for tuning. This value restricts the number of function
#'   evaluations. If set, must equal the size of the grid.
#' @return [\code{\link{TuneControlGrid}}]
#' @aliases TuneControlGrid
#' @family tune
#' @export
makeTuneControlGrid = function(same.resampling.instance = TRUE, impute.val = NULL,
  resolution = 10L, tune.threshold = FALSE, tune.threshold.args = list(),
  log.fun = "default", final.dw.perc = NULL, budget = NULL) {

  resolution = asCount(resolution)
  makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, resolution = resolution,
    tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget, cl = "TuneControlGrid")
}
