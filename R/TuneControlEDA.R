#' @export
#' @rdname TuneControl
makeTuneControlEDA = function(same.resampling.instance = TRUE, impute.val = NULL, start = NULL, 
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, final.dw.perc = NULL, 
  budget = NULL, ...) {
  
  ctrl = makeTuneControl(same.resampling.instance = same.resampling.instance, 
    impute.val = impute.val, start = start, tune.threshold = tune.threshold, 
    tune.threshold.args = tune.threshold.args, log.fun = log.fun, final.dw.perc = final.dw.perc, 
    budget = budget, ...,  cl = "TuneControlEDA")
    
  return(ctrl)
}