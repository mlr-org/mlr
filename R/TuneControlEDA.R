# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------

#' @export
#' @rdname TuneControl

makeTuneControlEDA = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL, 
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, 
  budget = NULL, ...) {

  args = list(...)

  # Default hyper-parameters that can be setable
  if (is.null(args$popSize)) {
  	args$popSize = 100L
  }

  if (is.null(args$maxit)) {
  	args$maxit = 50L
  }

  if (is.null(args$type) || args$type %nin% c("UMDA", "GCEDA", "CVEDA", "DVEDA")) {
   	args$type = "UMDA"
  }

  maxf = args$popSize * args$maxit

  if (is.null(budget)) {
      budget = maxf
  } else {
    if (maxf != budget) {
      stopf("The given budget (%i) contradicts to the maximum number of 
        function evaluations (max.call = %i).", budget, maxf)
    }
  }

  args2 = list(same.resampling.instance = same.resampling.instance, impute.val = impute.val, 
    start = start, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget, cl = "TuneControlEDA")
    
  ctrl = do.call(makeTuneControl, c(args, args2))
  return(ctrl)
}

# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------