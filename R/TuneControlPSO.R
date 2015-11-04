# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

#' @export
#' @rdname TuneControl

makeTuneControlPSO = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL, 
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, 
  budget = NULL, ...) {

  args = list(...)

  # Default hyper-parameters that can be setable
  if (is.null(args$type) || args$type %nin% c("SPSO2007", "SPSO2011")) {
    args$type = "SPSO2007"
  }
    
  if (is.null(args$maxit)) {
    args$maxit = 100L
  }

  if (is.null(args$nParticles)) {
    if (args$type == "SPSO2011") {
      args$nParticles = 40L
    } else {
      args$nParticles = 20L
    }
  }

  args$maxf = (args$nParticles * args$maxit)
  
  if (is.null(budget)) {
      budget = args$maxf
  } else {
    if (args$maxf != budget) {
      stopf("The given budget (%i) contradicts to the maximum number of 
       function evaluations (max.call = %i).", budget, args$maxf)
    }
  }

  args$trace = 0
  args$trace.stats = FALSE

  # mlr expected attributes
  args2 = list(same.resampling.instance = same.resampling.instance, impute.val = impute.val, 
    start = start, tune.threshold = tune.threshold, tune.threshold.args = tune.threshold.args,
    log.fun = log.fun, final.dw.perc = final.dw.perc, budget = budget, cl = "TuneControlPSO")
  
  ctrl = do.call(makeTuneControl, c(args, args2))
  return(ctrl)
}

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------