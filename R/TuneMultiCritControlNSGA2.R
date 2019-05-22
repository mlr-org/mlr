#' @export
#' @rdname TuneMultiCritControl
makeTuneMultiCritControlNSGA2 = function(same.resampling.instance = TRUE,
  impute.val = NULL, log.fun = "default", final.dw.perc = NULL, budget = NULL, ...) {

  args = list(...)
  if (is.null(args$popsize)) {
    if (!is.null(budget) && !is.null(args$generations)) {
      # define popsize via the number of generations and the budget
      args$popsize = budget %/% (max(args$generations) + 1L)
    } else {
      # alternatively use the nsga2-default
      args$popsize = 100L
    }
  }

  if (is.null(args$generations)) {
    if (!is.null(budget)) {
      # define generations via popsize and the budget
      args$generations = (budget %/% args$popsize) - 1L
    } else {
      # alternatively use the nsga2-default
      args$generations = 100L
    }
  }

  # adapt budget to the population size and number of generations
  if (is.null(budget)) {
    budget = (max(args$generations) + 1) * args$popsize
  } else if (budget != (max(args$generations) + 1) * args$popsize) {
    stopf("The given 'budget' (%i) contradicts the product of 'popsize' (%i) and 'max(generations) + 1' (%i)!",
      budget, args$popsize, args$generations + 1)
  }

  # sanity checks and type conversion
  args$popsize = asCount(args$popsize, positive = TRUE)
  args$generations = asInteger(args$generations, lower = 1L)
  budget = asCount(budget, positive = TRUE)

  args2 = list(same.resampling.instance = same.resampling.instance, impute.val = impute.val,
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget,
    cl = "TuneMultiCritControlNSGA2")
  do.call(makeTuneMultiCritControl, c(args, args2))
}
