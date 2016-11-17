#' @param prob.crossover [\code{numeric(1)}]\cr
#'   The GA probability of crossover between pairs of chromosomes.   
#'   Typically this is a large value. Default is 0.8.
#' @param prob.mutation [\code{numeric(1)}]\cr
#'   The GA probability of mutation in a parent chromosome. 
#'   Usually mutation occurs with a small probability.
#'   Default is 0.1.
#' @param pop.size [\code{integer(1)}]\cr
#'   Size of the initial population.
#'   Default is 50.
#' @export
#' @rdname TuneControl
makeTuneControlGA = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL,
  budget = NULL, prob.crossover = 0.8, prob.mutation = 0.1, pop.size = 50L, maxit = 100L, ...) {

  assertInt(x = maxit, lower = 1, .var.name = "maxit")
  assertInt(x = pop.size, lower = 1, .var.name = "pop.size")
  assertNumeric(x = prob.crossover, len = 1, lower = 0, upper = 1, .var.name = "prob.crossover")
  assertNumeric(x = prob.mutation, len = 1, lower = 0, upper = 1, .var.name = "prob.mutation")

  ctrl = makeTuneControl(same.resampling.instance = same.resampling.instance,
    impute.val = impute.val, start = start, tune.threshold = tune.threshold,
    tune.threshold.args = tune.threshold.args, log.fun = log.fun, final.dw.perc = final.dw.perc,
    budget = budget, prob.crossover = prob.crossover, prob.mutation = prob.mutation, 
    pop.size  = pop.size , maxit = maxit, ..., cl = "TuneControlGA")
  return(ctrl)
}
