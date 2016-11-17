#' @param eda.impl [\code{character}]\cr
#'   Implementation of the Copula EDA used. The possible values are: 
#'   UMDA - Univariate Marginal Distribution Algorithm, GCEDA - Gaussian Copula 
#'   Estimation of Distribution Algorithm, CVEDA - C-Vine EDA, and DVEDA - D-Vine EDA.
#'   Default is UMDA.
#' @export
#' @rdname TuneControl
makeTuneControlEDA = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL,
  budget = NULL, eda.impl = "UMDA", maxit = 100L, pop.size = 50L, ...) {

  assertChoice(x = eda.impl, choices = c("UMDA", "GCEDA", "CVEDA", "DVEDA"))
  assertInt(x = maxit, lower = 1, .var.name = "maxit")
  assertInt(x = pop.size, lower = 1, .var.name = "pop.size")

  ctrl = makeTuneControl(same.resampling.instance = same.resampling.instance,
    impute.val = impute.val, start = start, tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args, log.fun = log.fun, final.dw.perc = final.dw.perc,
    budget = budget, eda.impl = eda.impl, maxit = maxit, pop.size = pop.size, ..., cl = "TuneControlEDA")
  return(ctrl)
}
