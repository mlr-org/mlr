# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#' @export
#' @rdname TuneControl

makeTuneControlGA = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL, 
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, 
  budget = NULL, ...) {

  args = list(...)

  # Default hyper-parameters that can be setable
  if (is.null(args$pcrossover)) {
    args$pcrossover = 0.8
  }

  if (is.null(args$pmutation)) {
    args$pmutation = 0.1
  }

  if (is.null(args$parallel)) {
    args$parallel = FALSE
  }
    
  if (is.null(args$popSize)) {
    args$popSize = 50L
  }

  if (is.null(args$maxit)) {
    args$maxit = 100L
  }

  args$run = args$maxit
  maxf = args$popSize * args$maxit

  if (is.null(budget)) {
    budget = maxf
  } else {
    if (maxf != budget) {
      stopf("The given budget (%i) contradicts to the maximum number of function evaluations (maxf = %i).", 
        budget, maxf)
    }
  }

  args2 = list(same.resampling.instance = same.resampling.instance, impute.val = impute.val, 
    start = start, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args, 
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget, cl = "TuneControlGA")
    
  ctrl = do.call(makeTuneControl, c(args, args2))
  return(ctrl)
}

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------