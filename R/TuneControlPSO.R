#' @param pso.impl [\code{character}]\cr
#'   PSO algorithm implementation.
#'   The possible values are: SPSO2007 and SPSO2011.
#'   Default is SPSO2007.
#' @export
#' @rdname TuneControl
makeTuneControlPSO = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL,
  budget = NULL, pso.impl = "SPSO2007", maxit = 100L, ...) {

  assertChoice(x = pso.impl, choices = c("SPSO2011", "SPSO2007"), .var.name = "pso.impl")
  assertInt(x = maxit, lower = 1, .var.name = "maxit")

  ctrl = makeTuneControl(same.resampling.instance = same.resampling.instance,
    impute.val = impute.val, start = start, tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args, log.fun = log.fun, final.dw.perc = final.dw.perc,
    budget = budget, pso.impl = pso.impl, maxit = maxit, ..., cl = "TuneControlPSO")
  return(ctrl)
}
