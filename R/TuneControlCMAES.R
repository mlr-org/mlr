#' @title Create control object for hyperparameter tuning with CMAES.
#'
#' @description
#' CMA Evolution Strategy with method [cmaes::cma_es].
#' Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#' For integers the internally proposed numeric values are automatically rounded.
#' The sigma variance parameter is initialized to 1/4 of the span of box-constraints per
#' parameter dimension.
#'
#' @inherit TuneControl
#' @param budget (`integer(1)`)\cr
#'   Maximum budget for tuning. This value restricts the number of function
#'   evaluations. The `budget` corresponds to the product of the number of generations
#'   (`maxit`) and the number of offsprings per generation
#'   (`lambda`).
#' @return ([TuneControlCMAES])
#' @aliases TuneControlCMAES
#' @family tune
#' @export
makeTuneControlCMAES = function(same.resampling.instance = TRUE, impute.val = NULL,
  start = NULL, tune.threshold = FALSE, tune.threshold.args = list(), log.fun = "default",
  final.dw.perc = NULL, budget = NULL, ...) {
  ctrl = makeTuneControl(same.resampling.instance = same.resampling.instance,
    impute.val = impute.val, start = start, tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args, log.fun = log.fun,
    final.dw.perc = final.dw.perc, budget = budget, ..., cl = "TuneControlCMAES")

  return(ctrl)
}
