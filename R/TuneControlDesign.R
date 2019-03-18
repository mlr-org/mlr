#' @title Create control object for hyperparameter tuning with predefined design.
#'
#' @description
#' Completely pre-specifiy a `data.frame` of design points to be evaluated
#' during tuning. All kinds of parameter types can be handled.
#'
#' @inherit TuneControl
#' @param design ([data.frame])\cr
#'   `data.frame` containing the different parameter settings to be evaluated.
#'   The columns have to be named according to the `ParamSet` which will be used in `tune()`.
#'   Proper designs can be created with [ParamHelpers::generateDesign] for instance.
#' @return ([TuneControlDesign])
#' @aliases TuneControlDesign
#' @family tune
#' @export
makeTuneControlDesign = function(same.resampling.instance = TRUE, impute.val = NULL, design = NULL, tune.threshold = FALSE, tune.threshold.args = list(), log.fun = "default") {
  assertDataFrame(design, min.rows = 1)
  budget = nrow(design)
  makeTuneControl(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    start = NULL, design = design,
    tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, budget = budget, cl = "TuneControlDesign")
}
